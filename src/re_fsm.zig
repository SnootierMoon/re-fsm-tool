const std = @import("std");

pub const ReAst = struct {
    nodes: std.MultiArrayList(Node),

    pub const Node = union(enum) {
        epsilon: void,
        mask: u128,
        either: usize,
        concat: usize,
        look_around: LookAround,
        repeat,
    };

    pub const LookAround = struct {
        dir: Direction,
        sign: Sign,

        const Direction = enum { ahead, behind };
        const Sign = enum { pos, neg };

        fn print(look_around: LookAround) []const u8 {
            if (look_around.dir == .ahead and look_around.sign == .pos) {
                return "positive lookahead";
            } else if (look_around.dir == .ahead and look_around.sign == .neg) {
                return "negative lookahead";
            } else if (look_around.dir == .behind and look_around.sign == .pos) {
                return "positive lookbehind";
            } else if (look_around.dir == .behind and look_around.sign == .neg) {
                return "negative lookbehind";
            }
            unreachable;
        }
    };

    fn isSpecial(ch: u8) bool {
        return switch (ch) {
            '(', ')', '*', '+', '.', '?', '[', '\\', ']', '{', '|', '}' => true,
            else => false,
        };
    }

    fn isLiteral(ch: u8) bool {
        return std.ascii.isAscii(ch) and !isSpecial(ch);
    }

    fn nodeStartIdx(nodes: *std.MultiArrayList(Node)) usize {
        var idx = nodes.len - 1;
        const slice = nodes.slice();
        return while (true) {
            switch (slice.get(idx)) {
                .epsilon, .mask => break idx,
                .either => |i| idx = i,
                .concat => |i| idx = i,
                .look_around, .repeat => idx = idx - 1,
            }
        };
    }

    fn copyNode(gpa: std.mem.Allocator, nodes: *std.MultiArrayList(Node), start: usize, end: usize) !void {
        try nodes.ensureUnusedCapacity(gpa, end - start);
        const slice = nodes.slice();
        const tags = slice.items(.tags);
        const data = slice.items(.data);
        const to = nodes.len;
        for (start..end) |idx| {
            switch (tags[idx]) {
                .epsilon => nodes.appendAssumeCapacity(.{ .epsilon = {} }),
                .mask => nodes.appendAssumeCapacity(.{ .mask = data[idx].mask }),
                .either => nodes.appendAssumeCapacity(.{ .either = data[idx].either - start + to }),
                .concat => nodes.appendAssumeCapacity(.{ .concat = data[idx].concat - start + to }),
                .look_around => nodes.appendAssumeCapacity(.{ .look_around = data[idx].look_around }),
                .repeat => nodes.appendAssumeCapacity(.{ .repeat = {} }),
            }
        }
    }

    fn genRepeat(gpa: std.mem.Allocator, nodes: *std.MultiArrayList(Node), low: usize, high: ?usize) !void {
        const inner_start = nodeStartIdx(nodes);
        const inner_end = nodes.len;
        if (high) |high_| {
            std.debug.assert(low <= high_);
            if (high_ == 0) {
                nodes.len = inner_start;
                try nodes.append(gpa, .{ .epsilon = {} });
            } else if (low == high_) {
                for (1..low) |_| {
                    const idx = nodes.len - 1;
                    try copyNode(gpa, nodes, inner_start, inner_end);
                    try nodes.append(gpa, .{ .concat = idx });
                }
            } else {
                try nodes.append(gpa, .{ .epsilon = {} });
                try nodes.append(gpa, .{ .either = nodes.len - 2 });
                for (low + 1..high_) |_| {
                    const idx = nodes.len - 1;
                    try copyNode(gpa, nodes, inner_start, inner_end);
                    try nodes.append(gpa, .{ .concat = idx });
                    try nodes.append(gpa, .{ .epsilon = {} });
                    try nodes.append(gpa, .{ .either = nodes.len - 2 });
                }
                for (0..low) |_| {
                    const idx = nodes.len - 1;
                    try copyNode(gpa, nodes, inner_start, inner_end);
                    try nodes.append(gpa, .{ .concat = idx });
                }
            }
        } else if (low == 0) {
            try nodes.append(gpa, .{ .repeat = {} });
            try nodes.append(gpa, .{ .epsilon = {} });
            try nodes.append(gpa, .{ .either = nodes.len - 2 });
        } else {
            try nodes.append(gpa, .{ .repeat = {} });
            for (1..low) |_| {
                const idx = nodes.len - 1;
                try copyNode(gpa, nodes, inner_start, inner_end);
                try nodes.append(gpa, .{ .concat = idx });
            }
        }
    }

    const Stream = struct {
        str: []const u8,
        idx: usize = 0,

        fn done(s: Stream) bool {
            return s.idx >= s.str.len;
        }

        fn peek(s: Stream) ?u8 {
            if (!s.done()) {
                return s.str[s.idx];
            } else {
                return null;
            }
        }

        fn getch(s: *Stream) ?u8 {
            if (!s.done()) {
                defer s.idx += 1;
                return s.str[s.idx];
            } else {
                return null;
            }
        }

        fn getchIf(s: *Stream, predicate: fn (u8) bool) ?u8 {
            if (!s.done() and predicate(s.str[s.idx])) {
                defer s.idx += 1;
                return s.str[s.idx];
            } else {
                return null;
            }
        }

        fn getchIfEq(s: *Stream, ch: u8) bool {
            if (!s.done() and s.str[s.idx] == ch) {
                defer s.idx += 1;
                return true;
            } else {
                return false;
            }
        }

        fn getchIfNeq(s: *Stream, ch: u8) ?u8 {
            if (!s.done() and s.str[s.idx] != ch) {
                defer s.idx += 1;
                return s.str[s.idx];
            } else {
                return null;
            }
        }
    };

    fn parseInteger(comptime T: type, stream: *Stream) !?T {
        if (stream.getchIf(std.ascii.isDigit)) |ch0| {
            var int: T = ch0 - '0';
            while (stream.getchIf(std.ascii.isDigit)) |ch| {
                int = try std.math.add(T, try std.math.mul(T, int, 10), ch - '0');
            }
            return int;
        } else {
            return null;
        }
    }

    fn parseCharClass(stream: *Stream) !u128 {
        var negated = false;
        var last: ?u8 = undefined;
        var class_mask: u128 = 0;
        if (stream.getchIfEq('^')) {
            negated = true;
        }
        if (stream.getchIfEq(']')) {
            last = '-';
            class_mask |= 1 << ']';
        }
        while (stream.getchIfNeq(']')) |ch| {
            if (ch == '-') {
                if (stream.done()) {
                    return error.Parse;
                } else if (stream.peek() != ']') {
                    if (stream.getch()) |next_ch| {
                        if (last) |last_ch| {
                            class_mask &= ~(@as(u128, 1) << @intCast(last_ch));
                            const lo = @min(last_ch, next_ch);
                            const hi = @max(last_ch, next_ch);
                            class_mask |= (std.math.shl(u128, 1, hi - lo + 1) -% 1) << @intCast(lo);
                            continue;
                        }
                    } else {
                        return error.Parse;
                    }
                }
            } else if (!std.ascii.isAscii(ch)) {
                return error.Parse;
            }
            last = ch;
            class_mask |= @as(u128, 1) << @intCast(ch);
        }
        if (stream.getch() != ']') {
            return error.Parse;
        }
        return if (negated) ~class_mask else class_mask;
    }

    fn parseQuantifier(gpa: std.mem.Allocator, nodes: *std.MultiArrayList(Node), stream: *Stream) !void {
        if (stream.getchIfEq('*')) {
            try genRepeat(gpa, nodes, 0, null);
        } else if (stream.getchIfEq('+')) {
            try genRepeat(gpa, nodes, 1, null);
        } else if (stream.getchIfEq('?')) {
            try genRepeat(gpa, nodes, 0, 1);
        } else if (stream.getchIfEq('{')) {
            const low = try parseInteger(usize, stream);
            if (stream.getchIfEq('}')) {
                if (low) |low_| {
                    try genRepeat(gpa, nodes, low_, low_);
                } else {
                    return error.Parse;
                }
            } else if (stream.getchIfEq(',')) {
                const high = try parseInteger(usize, stream);
                if (stream.getchIfEq('}')) {
                    try genRepeat(gpa, nodes, low orelse 0, high);
                } else {
                    return error.Parse;
                }
            } else {
                return error.Parse;
            }
        }
    }

    pub fn parse(gpa: std.mem.Allocator, str: []const u8) !ReAst {
        const StackFrame = struct {
            term: ?usize = null,
            expr: ?usize = null,
            look_around: ?LookAround = null,

            fn addAtomToTerm(
                frame: *@This(),
                gpa_: std.mem.Allocator,
                nodes: *std.MultiArrayList(Node),
            ) !void {
                if (frame.term) |atom| {
                    try nodes.append(gpa_, .{ .concat = atom });
                }
                frame.term = nodes.len - 1;
            }

            fn addTermToExpr(
                frame: *@This(),
                gpa_: std.mem.Allocator,
                nodes: *std.MultiArrayList(Node),
            ) !void {
                if (frame.term == null) {
                    try nodes.append(gpa_, .{ .epsilon = {} });
                }
                if (frame.expr) |expr| {
                    try nodes.append(gpa_, .{ .either = expr });
                }
                frame.expr = nodes.len - 1;
                frame.term = null;
            }
        };

        var stream: Stream = .{ .str = str };
        var nodes: std.MultiArrayList(Node) = .{};
        errdefer nodes.deinit(gpa);

        var stack = std.ArrayList(StackFrame).init(gpa);
        defer stack.deinit();
        var curr_frame = StackFrame{};

        while (!stream.done()) {
            if (stream.getchIf(isLiteral)) |ch| {
                try nodes.append(gpa, .{ .mask = @as(u128, 1) << @intCast(ch) });
                try parseQuantifier(gpa, &nodes, &stream);
                try curr_frame.addAtomToTerm(gpa, &nodes);
            } else if (stream.getchIfEq('\\')) {
                if (stream.getchIf(isSpecial)) |ch| {
                    try nodes.append(gpa, .{ .mask = @as(u128, 1) << @intCast(ch) });
                    try parseQuantifier(gpa, &nodes, &stream);
                    try curr_frame.addAtomToTerm(gpa, &nodes);
                }
            } else if (stream.getchIfEq('.')) {
                try nodes.append(gpa, .{ .mask = std.math.maxInt(u128) });
                try parseQuantifier(gpa, &nodes, &stream);
                try curr_frame.addAtomToTerm(gpa, &nodes);
            } else if (stream.getchIfEq('[')) {
                try nodes.append(gpa, .{ .mask = try parseCharClass(&stream) });
                try parseQuantifier(gpa, &nodes, &stream);
                try curr_frame.addAtomToTerm(gpa, &nodes);
            } else if (stream.getchIfEq('|')) {
                try curr_frame.addTermToExpr(gpa, &nodes);
            } else if (stream.getchIfEq('(')) {
                var look_around: ?LookAround = null;
                if (stream.getchIfEq('?')) {
                    look_around = LookAround{ .dir = .ahead, .sign = undefined };
                    if (stream.getchIfEq('<')) {
                        look_around.?.dir = .behind;
                    }
                    if (stream.getchIfEq('!')) {
                        look_around.?.sign = .neg;
                    } else if (stream.getchIfEq('=')) {
                        look_around.?.sign = .pos;
                    } else {
                        return error.Parse;
                    }
                }
                try stack.append(curr_frame);
                curr_frame = StackFrame{ .look_around = look_around };
            } else if (stream.getchIfEq(')')) {
                var old_frame = stack.popOrNull() orelse return error.Parse;
                try parseQuantifier(gpa, &nodes, &stream);
                try curr_frame.addTermToExpr(gpa, &nodes);
                try old_frame.addAtomToTerm(gpa, &nodes);
                if (curr_frame.look_around) |look_around| {
                    try nodes.append(gpa, .{ .look_around = look_around });
                }
                curr_frame = old_frame;
            } else {
                return error.Parse;
            }
        }
        try curr_frame.addTermToExpr(gpa, &nodes);

        if (!stream.done() or stack.items.len != 0) {
            return error.Parse;
        }
        return .{ .nodes = nodes };
    }

    pub fn deinit(ast: *ReAst, gpa: std.mem.Allocator) void {
        ast.nodes.deinit(gpa);
    }

    fn printRange(writer: anytype, first: bool, start: u8, end: u8) !void {
        if (!first) {
            try writer.print(",", .{});
        }
        if (start == '\'' or start == '\"') {
            try writer.print("'\\{c}'", .{start});
        } else if (start == '\\') {
            try writer.print("'\\\\{c}'", .{start});
        } else {
            try writer.print("'{c}'", .{start});
        }
        if (start != end) {
            try writer.print("-", .{});
            if (end == '\'' or end == '\"') {
                try writer.print("'\\{c}'", .{end});
            } else if (end == '\\') {
                try writer.print("'\\\\{c}'", .{end});
            } else {
                try writer.print("'{c}'", .{end});
            }
        }
    }

    fn printMask(writer: anytype, mask: u128) !void {
        if (mask == 0) {
            try writer.print("fail", .{});
        } else if (mask == std.math.maxInt(u128)) {
            try writer.print(".", .{});
        } else {
            var first = true;
            var start: ?u8 = null;
            for (32..128) |i| {
                if (mask & (@as(u128, 1) << @intCast(i)) != 0) {
                    if (start == null) {
                        start = @as(u8, @intCast(i));
                    }
                } else if (start) |start_| {
                    const end: u8 = @as(u8, @intCast(i)) - 1;
                    try printRange(writer, first, start_, end);
                    first = false;
                    start = null;
                }
            }
            if (start) |start_| {
                try printRange(writer, first, start_, '~');
            }
        }
    }

    pub fn viz(ast: ReAst, writer: anytype) !void {
        const slice = ast.nodes.slice();
        const tags = slice.items(.tags);
        const data = slice.items(.data);
        try writer.print("digraph {{", .{});
        try writer.print(" node [shape=circle]", .{});
        for (tags, 0..ast.nodes.len) |tag, idx| {
            switch (tag) {
                .epsilon => try writer.print(" {} [label=\"epsilon\"]", .{idx}),
                .mask => {
                    try writer.print(" {} [label=\"", .{idx});
                    try printMask(writer, data[idx].mask);
                    try writer.print("\"]", .{});
                },
                .either => try writer.print(" {} [label=\"union\"]", .{idx}),
                .concat => try writer.print(" {} [label=\"concat\"]", .{idx}),
                .look_around => try writer.print(" {} [label=\"{s}\"]", .{ idx, data[idx].look_around.print() }),
                .repeat => try writer.print(" {} [label=\"repeat\"]", .{idx}),
            }
        }
        for (tags, 0..ast.nodes.len) |tag, idx| {
            switch (tag) {
                .epsilon, .mask => {},
                .either => {
                    try writer.print(" {} -> {}", .{ idx, idx - 1 });
                    try writer.print(" {} -> {}", .{ idx, data[idx].either });
                },
                .concat => {
                    try writer.print(" {} -> {}", .{ idx, idx - 1 });
                    try writer.print(" {} -> {}", .{ idx, data[idx].concat });
                },
                .look_around, .repeat => {
                    try writer.print(" {} -> {}", .{ idx, idx - 1 });
                },
            }
        }
        try writer.print(" }}", .{});
    }
};

pub const Nfa = struct {
    states: usize,
    final0: usize,
    edges: []Edge,

    const Edge = struct {
        from: usize,
        sym: ?u7,
        to: usize,
    };

    pub fn fromReAst(gpa: std.mem.Allocator, ast: ReAst) !Nfa {
        var edges = std.ArrayList(Edge).init(gpa);
        defer edges.deinit();

        const StateInfo = struct { n_states: usize, state0: usize, n_finals: usize, final0: usize };
        var infos = try gpa.alloc(StateInfo, ast.nodes.len);
        defer gpa.free(infos);

        const slice = ast.nodes.slice();
        const tags = slice.items(.tags);
        const data = slice.items(.data);
        for (tags, 0..ast.nodes.len) |tag, idx| {
            switch (tag) {
                .epsilon => {
                    infos[idx].n_states = 2;
                    infos[idx].n_finals = 1;
                },
                .mask => {
                    infos[idx].n_states = 2;
                    infos[idx].n_finals = 1;
                },
                .either => {
                    const lhs_idx = data[idx].either;
                    infos[idx].n_states = 1 + infos[lhs_idx].n_states + infos[idx - 1].n_states;
                    infos[idx].n_finals = infos[lhs_idx].n_finals + infos[idx - 1].n_finals;
                },
                .concat => {
                    const lhs_idx = data[idx].concat;
                    infos[idx].n_states = infos[lhs_idx].n_states + infos[idx - 1].n_states;
                    infos[idx].n_finals = infos[idx - 1].n_finals;
                },
                .look_around => return error.Unimplemented,
                .repeat => {
                    infos[idx].n_states = infos[idx - 1].n_states;
                    infos[idx].n_finals = infos[idx - 1].n_finals;
                },
            }
        }
        infos[ast.nodes.len - 1].state0 = 0;
        infos[ast.nodes.len - 1].final0 = infos[ast.nodes.len - 1].n_states - infos[ast.nodes.len - 1].n_finals;
        for (0..ast.nodes.len) |i| {
            const idx = ast.nodes.len - i - 1;
            switch (tags[idx]) {
                .epsilon, .mask => {},
                .either => {
                    const lhs_idx = data[idx].either;
                    infos[lhs_idx].state0 = infos[idx].state0 + 1;
                    infos[lhs_idx].final0 = infos[idx].final0;
                    infos[idx - 1].state0 = infos[idx].state0 + infos[lhs_idx].n_states - infos[lhs_idx].n_finals + 1;
                    infos[idx - 1].final0 = infos[idx].final0 + infos[lhs_idx].n_finals;
                },
                .concat => {
                    const lhs_idx = data[idx].concat;
                    infos[lhs_idx].state0 = infos[idx].state0;
                    infos[lhs_idx].final0 = infos[idx].state0 + infos[lhs_idx].n_states - infos[lhs_idx].n_finals;
                    infos[idx - 1].state0 = infos[idx].state0 + infos[lhs_idx].n_states;
                    infos[idx - 1].final0 = infos[idx].final0;
                },
                .look_around => return error.Unimplemented,
                .repeat => {
                    infos[idx - 1].state0 = infos[idx].state0;
                    infos[idx - 1].final0 = infos[idx].final0;
                },
            }
        }

        for (0..ast.nodes.len) |idx| {
            switch (tags[idx]) {
                .epsilon => {
                    try edges.append(.{ .from = infos[idx].state0, .sym = null, .to = infos[idx].final0 });
                },
                .mask => {
                    for (0..128) |i| {
                        if (data[idx].mask & (@as(u128, 1) << @intCast(i)) != 0) {
                            try edges.append(.{ .from = infos[idx].state0, .sym = @intCast(i), .to = infos[idx].final0 });
                        }
                    }
                },
                .either => {
                    const lhs_idx = data[idx].either;
                    try edges.append(.{ .from = infos[idx].state0, .sym = null, .to = infos[lhs_idx].state0 });
                    try edges.append(.{ .from = infos[idx].state0, .sym = null, .to = infos[idx - 1].state0 });
                },
                .concat => {
                    const lhs_idx = data[idx].concat;
                    for (0..infos[lhs_idx].n_finals) |i| {
                        try edges.append(.{ .from = infos[lhs_idx].final0 + i, .sym = null, .to = infos[idx - 1].state0 });
                    }
                },
                .look_around => return error.Unimplemented,
                .repeat => {
                    for (0..infos[idx - 1].n_finals) |i| {
                        try edges.append(.{ .from = infos[idx - 1].final0 + i, .sym = null, .to = infos[idx - 1].state0 });
                    }
                },
            }
        }

        return .{
            .states = infos[ast.nodes.len - 1].n_states,
            .final0 = infos[ast.nodes.len - 1].final0,
            .edges = try edges.toOwnedSlice(),
        };
    }

    pub fn deinit(nfa: *Nfa, gpa: std.mem.Allocator) void {
        gpa.free(nfa.edges);
    }

    pub fn viz(nfa: Nfa, writer: anytype) !void {
        try writer.print("digraph {{", .{});
        try writer.print(" node [shape=circle]", .{});
        for (0..nfa.final0) |i| {
            try writer.print(" {}\n", .{i});
        }
        try writer.print(" node [shape=rect]", .{});
        for (nfa.final0..nfa.states) |i| {
            try writer.print(" {}\n", .{i});
        }
        for (nfa.edges) |edge| {
            if (edge.sym) |sym| {
                try writer.print(" {} -> {} [label=\"{c}\"]", .{ edge.from, edge.to, sym });
            } else {
                try writer.print(" {} -> {}", .{ edge.from, edge.to });
            }
        }
        try writer.print(" }}", .{});
    }
};

pub const Dfa = struct {
    edges: [][128]usize,

    const set = struct {
        fn find(items: []const usize, key: usize) struct { bool, usize } {
            var lo: usize = 0;
            var hi = items.len;
            while (lo < hi) {
                const mid = lo + (hi - lo) / 2;
                if (items[mid] >= key) {
                    hi = mid;
                } else {
                    lo = mid + 1;
                }
            }
            return .{ lo < items.len and items[lo] == key, lo };
        }

        fn insert(arena: std.mem.Allocator, s: *std.ArrayListUnmanaged(usize), key: usize) !bool {
            const found, const idx = find(s.items, key);
            if (!found) {
                try s.insert(arena, idx, key);
                return true;
            }
            return false;
        }

        fn includes(lhs: []const usize, rhs: []const usize) bool {
            return for (lhs) |i| {
                const found, _ = find(rhs, i);
                if (!found) {
                    break false;
                }
            } else true;
        }

        fn equals(lhs: []const usize, rhs: []const usize) bool {
            return lhs.len == rhs.len and includes(lhs, rhs);
        }
    };

    const NfaEdges = struct {
        map: Map,

        const Map = std.AutoHashMapUnmanaged(struct { usize, ?u7 }, std.ArrayListUnmanaged(usize));

        fn init(arena: std.mem.Allocator, edges: []Nfa.Edge) !NfaEdges {
            var map = Map{};
            for (edges) |edge| {
                const gop = try map.getOrPut(arena, .{ edge.from, edge.sym });
                if (!gop.found_existing) {
                    gop.value_ptr.* = .{};
                }
                try gop.value_ptr.append(arena, edge.to);
            }

            return .{ .map = map };
        }

        fn eClosure(nfa_edges: *NfaEdges, arena: std.mem.Allocator, states: *std.ArrayListUnmanaged(usize)) !void {
            var pq = std.PriorityQueue(usize, void, struct {
                fn f(ctx: void, lhs: usize, rhs: usize) std.math.Order {
                    _ = ctx;
                    return std.math.order(rhs, lhs);
                }
            }.f).init(arena, {});
            try pq.addSlice(states.items);
            while (pq.removeOrNull()) |state| {
                if (nfa_edges.map.get(.{ state, null })) |to_list| {
                    for (to_list.items) |to| {
                        if (try set.insert(arena, states, to)) {
                            try pq.add(to);
                        }
                    }
                }
            }
        }

        fn move(nfa_edges: NfaEdges, arena: std.mem.Allocator, states: []const usize, sym: u7) !std.ArrayListUnmanaged(usize) {
            var result: std.ArrayListUnmanaged(usize) = .{};
            for (states) |state| {
                if (nfa_edges.map.get(.{ state, sym })) |to_list| {
                    for (to_list.items) |to| {
                        _ = try set.insert(arena, &result, to);
                    }
                }
            }
            return result;
        }
    };

    pub fn fromNfa(gpa: std.mem.Allocator, nfa: Nfa) !Dfa {
        var arena_obj = std.heap.ArenaAllocator.init(gpa);
        defer arena_obj.deinit();
        const arena = arena_obj.allocator();

        var nfa_edges = try NfaEdges.init(arena, nfa.edges);
        var state_sets: std.ArrayListUnmanaged([]usize) = .{};

        var edges: std.ArrayListUnmanaged([128]usize) = .{};
        defer edges.deinit(gpa);

        var state0: std.ArrayListUnmanaged(usize) = .{};
        try state0.append(arena, 0);
        try nfa_edges.eClosure(arena, &state0);
        try state_sets.append(arena, try state0.toOwnedSlice(arena));

        var i: usize = 0;
        while (i < state_sets.items.len) : (i += 1) {
            const state_edges = try edges.addOne(gpa);
            for (0..128) |sym| {
                var new_state = try nfa_edges.move(arena, state_sets.items[i], @intCast(sym));
                try nfa_edges.eClosure(arena, &new_state);

                const idx = for (0..state_sets.items.len) |j| {
                    if (std.mem.eql(usize, new_state.items, state_sets.items[j])) {
                        break j;
                    }
                } else blk: {
                    try state_sets.append(arena, try new_state.toOwnedSlice(arena));
                    break :blk state_sets.items.len - 1;
                };

                state_edges[sym] = idx;
            }
        }

        return .{ .edges = try edges.toOwnedSlice(gpa) };
    }

    pub fn deinit(dfa: Dfa, gpa: std.mem.Allocator) void {
        gpa.free(dfa.edges);
    }
};
