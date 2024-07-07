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
        var nodes = std.MultiArrayList(Node){};
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
                std.log.info("{}", .{nodes.items(.data)[nodes.len-1].mask});
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
    deltas: []Delta,

    const Delta = struct {
        from: usize,
        sym: ?u8,
        to: usize,
    };

    pub fn init(gpa: std.mem.Allocator, ast: ReAst) !Nfa {
        var arena_obj = std.heap.ArenaAllocator.init(gpa);
        defer arena_obj.deinit();
        const arena = arena_obj.allocator();

        var deltas = std.ArrayList(Delta).init(gpa);
        defer deltas.deinit();

        const StateInfo = struct { n_states: usize, state0: usize, n_finals: usize, final0: usize };
        var infos = try arena.alloc(StateInfo, ast.nodes.len);

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
                    try deltas.append(.{ .from = infos[idx].state0, .sym = null, .to = infos[idx].final0 });
                },
                .mask => {
                    for (0..128) |i| {
                        if (data[idx].mask & (@as(u128, 1) << @intCast(i)) != 0) {
                            try deltas.append(.{ .from = infos[idx].state0, .sym = @intCast(i), .to = infos[idx].final0 });
                        }
                    }
                },
                .either => {
                    const lhs_idx = data[idx].either;
                    try deltas.append(.{ .from = infos[idx].state0, .sym = null, .to = infos[lhs_idx].state0 });
                    try deltas.append(.{ .from = infos[idx].state0, .sym = null, .to = infos[idx - 1].state0 });
                },
                .concat => {
                    const lhs_idx = data[idx].concat;
                    for (0..infos[lhs_idx].n_finals) |i| {
                        try deltas.append(.{ .from = infos[lhs_idx].final0 + i, .sym = null, .to = infos[idx - 1].state0 });
                    }
                },
                .look_around => return error.Unimplemented,
                .repeat => {
                    for (0..infos[idx - 1].n_finals) |i| {
                        try deltas.append(.{ .from = infos[idx - 1].final0 + i, .sym = null, .to = infos[idx - 1].state0 });
                    }
                },
            }
        }

        return .{
            .states = infos[ast.nodes.len - 1].n_states,
            .final0 = infos[ast.nodes.len - 1].final0,
            .deltas = try deltas.toOwnedSlice(),
        };
    }

    pub fn deinit(nfa: *Nfa, gpa: std.mem.Allocator) void {
        gpa.free(nfa.deltas);
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
        for (nfa.deltas) |delta| {
            if (delta.sym) |sym| {
                try writer.print(" {} -> {} [label=\"{c}\"]", .{ delta.from, delta.to, sym });
            } else {
                try writer.print(" {} -> {}", .{ delta.from, delta.to });
            }
        }
        try writer.print(" }}", .{});
    }
};

const Dfa = struct {
    delta: [][128]usize,
    final: std.DynamicBitSetUnmanaged,
};
