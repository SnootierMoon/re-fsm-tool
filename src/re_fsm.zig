const std = @import("std");

pub const ReAst = struct {
    nodes: std.MultiArrayList(Node),

    pub const Node = union(enum) {
        epsilon: void,
        mask: u95,
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

    fn popANode(nodes: *std.MultiArrayList(Node)) void {
        var idx = nodes.len - 1;
        const tags = nodes.items(.tags);
        const data = nodes.items(.data);
        nodes.len = while (true) {
            switch (tags[idx]) {
                .epsilon, .mask => break idx,
                .either => idx = data[idx].either,
                .concat => idx = data[idx].concat,
                .look_around, .repeat => idx = idx - 1,
            }
        };
    }

    fn genRepeat(allocator: std.mem.Allocator, nodes: *std.MultiArrayList(Node), low: usize, high: ?usize) !void {
        const idx = nodes.len - 1;
        if (high) |high_| {
            std.debug.assert(low <= high_);
            if (high_ == 0) {
                popANode(nodes);
                try nodes.append(allocator, .{ .epsilon = {} });
            } else if (low == high_) {
                for (1..low) |_| {
                    try nodes.append(allocator, .{ .concat = idx });
                }
            } else {
                try nodes.append(allocator, .{ .epsilon = {} });
                try nodes.append(allocator, .{ .either = idx });
                for (low + 1..high_) |_| {
                    try nodes.append(allocator, .{ .concat = idx });
                    try nodes.append(allocator, .{ .epsilon = {} });
                    try nodes.append(allocator, .{ .either = nodes.len - 2 });
                }
                for (0..low) |_| {
                    try nodes.append(allocator, .{ .concat = idx });
                }
            }
        } else if (low == 0) {
            try nodes.append(allocator, .{ .repeat = {} });
            try nodes.append(allocator, .{ .epsilon = {} });
            try nodes.append(allocator, .{ .either = nodes.len - 2 });
        } else {
            try nodes.append(allocator, .{ .repeat = {} });
            for (1..low) |_| {
                try nodes.append(allocator, .{ .concat = idx });
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

    fn parseCharClass(stream: *Stream) !u95 {
        var negated = false;
        var last: ?u8 = undefined;
        var class_mask: u95 = 0;
        if (stream.getchIfEq('^')) {
            negated = true;
        }
        if (stream.getchIfEq(']')) {
            last = 61;
            class_mask |= 1 << 61;
        }
        while (stream.getchIfNeq(']')) |ch| {
            if (ch == '-') {
                if (stream.done()) {
                    return error.Parse;
                } else if (stream.peek() != ']') {
                    if (stream.getch()) |next_ch| {
                        if (last) |last_ch| {
                            class_mask &= ~(@as(u95, 1) << @intCast(last_ch - ' '));
                            const lo = @min(last_ch, next_ch);
                            const hi = @max(last_ch, next_ch);
                            const mask = (std.math.shr(u95, 1, hi - lo + 1) -% 1) << @intCast(lo - ' ');
                            class_mask |= mask;
                            continue;
                        }
                    } else {
                        return error.Parse;
                    }
                }
            }
            last = ch;
            class_mask |= @as(u95, 1) << @intCast(ch - ' ');
        }
        if (stream.getch() != ']') {
            return error.Parse;
        }
        return if (negated) ~class_mask else class_mask;
    }

    fn parseQuantifier(allocator: std.mem.Allocator, nodes: *std.MultiArrayList(Node), stream: *Stream) !void {
        if (stream.getchIfEq('*')) {
            try genRepeat(allocator, nodes, 0, null);
        } else if (stream.getchIfEq('+')) {
            try genRepeat(allocator, nodes, 1, null);
        } else if (stream.getchIfEq('?')) {
            try genRepeat(allocator, nodes, 0, 1);
        } else if (stream.getchIfEq('{')) {
            const low = try parseInteger(usize, stream);
            if (stream.getchIfEq('}')) {
                if (low) |low_| {
                    try genRepeat(allocator, nodes, low_, low_);
                } else {
                    return error.Parse;
                }
            } else if (stream.getchIfEq(',')) {
                const high = try parseInteger(usize, stream);
                if (stream.getchIfEq('}')) {
                    try genRepeat(allocator, nodes, low orelse 0, high);
                } else {
                    return error.Parse;
                }
            } else {
                return error.Parse;
            }
        }
    }

    pub fn parse(allocator: std.mem.Allocator, str: []const u8) !ReAst {
        const StackFrame = struct {
            term: ?usize = null,
            expr: ?usize = null,
            look_around: ?LookAround = null,

            fn addAtomToTerm(
                frame: *@This(),
                allocator_: std.mem.Allocator,
                nodes: *std.MultiArrayList(Node),
            ) !void {
                if (frame.term) |atom| {
                    try nodes.append(allocator_, .{ .concat = atom });
                }
                frame.term = nodes.len - 1;
            }

            fn addTermToExpr(
                frame: *@This(),
                allocator_: std.mem.Allocator,
                nodes: *std.MultiArrayList(Node),
            ) !void {
                if (frame.term == null) {
                    try nodes.append(allocator_, .{ .epsilon = {} });
                }
                if (frame.expr) |expr| {
                    try nodes.append(allocator_, .{ .either = expr });
                }
                frame.expr = nodes.len - 1;
                frame.term = null;
            }
        };

        var stream: Stream = .{ .str = str };
        var nodes = std.MultiArrayList(Node){};
        errdefer nodes.deinit(allocator);

        var stack = std.ArrayList(StackFrame).init(allocator);
        defer stack.deinit();
        var curr_frame = StackFrame{};

        while (!stream.done()) {
            if (stream.getchIf(isLiteral)) |ch| {
                try nodes.append(allocator, .{ .mask = @as(u95, 1) << @intCast(ch - ' ') });
                try parseQuantifier(allocator, &nodes, &stream);
                try curr_frame.addAtomToTerm(allocator, &nodes);
            } else if (stream.getchIfEq('\\')) {
                if (stream.getchIf(isSpecial)) |ch| {
                    try nodes.append(allocator, .{ .mask = @as(u95, 1) << @intCast(ch - ' ') });
                    try parseQuantifier(allocator, &nodes, &stream);
                    try curr_frame.addAtomToTerm(allocator, &nodes);
                }
            } else if (stream.getchIfEq('.')) {
                try nodes.append(allocator, .{ .mask = std.math.maxInt(u95) });
                try parseQuantifier(allocator, &nodes, &stream);
                try curr_frame.addAtomToTerm(allocator, &nodes);
            } else if (stream.getchIfEq('[')) {
                try nodes.append(allocator, .{ .mask = try parseCharClass(&stream) });
                try parseQuantifier(allocator, &nodes, &stream);
                try curr_frame.addAtomToTerm(allocator, &nodes);
            } else if (stream.getchIfEq('|')) {
                try curr_frame.addTermToExpr(allocator, &nodes);
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
                try parseQuantifier(allocator, &nodes, &stream);
                try curr_frame.addTermToExpr(allocator, &nodes);
                try old_frame.addAtomToTerm(allocator, &nodes);
                if (curr_frame.look_around) |look_around| {
                    try nodes.append(allocator, .{ .look_around = look_around });
                }
                curr_frame = old_frame;
            } else {
                return error.Parse;
            }
        }
        try curr_frame.addTermToExpr(allocator, &nodes);

        if (!stream.done() or stack.items.len != 0) {
            return error.Parse;
        }
        return .{ .nodes = nodes };
    }

    pub fn deinit(ast: *ReAst, allocator: std.mem.Allocator) void {
        ast.nodes.deinit(allocator);
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

    fn printMask(writer: anytype, mask: u95) !void {
        if (mask == 0) {
            try writer.print("fail", .{});
        } else if (mask == std.math.maxInt(u95)) {
            try writer.print(".", .{});
        } else {
            var first = true;
            var start: ?u8 = null;
            for (0..95) |i| {
                if (mask & (@as(u95, 1) << @intCast(i)) != 0) {
                    if (start == null) {
                        start = @as(u8, @intCast(i)) + ' ';
                    }
                } else if (start) |start_| {
                    const end: u8 = @as(u8, @intCast(i)) + ' ' - 1;
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
        try writer.print("digraph {{", .{});
        try writer.print(" node [shape=circle]", .{});
        for (ast.nodes.items(.tags), ast.nodes.items(.data), 0..ast.nodes.len) |tag, data, i| {
            switch (tag) {
                .epsilon => try writer.print(" {} [label=\"epsilon\"]", .{i}),
                .mask => {
                    try writer.print(" {} [label=\"", .{i});
                    try printMask(writer, data.mask);
                    try writer.print("\"]", .{});
                },
                .either => try writer.print(" {} [label=\"union\"]", .{i}),
                .concat => try writer.print(" {} [label=\"concat\"]", .{i}),
                .look_around => try writer.print(" {} [label=\"{s}\"]", .{ i, data.look_around.print() }),
                .repeat => try writer.print(" {} [label=\"repeat\"]", .{i}),
            }
        }
        for (ast.nodes.items(.tags), ast.nodes.items(.data), 0..ast.nodes.len) |tag, data, i| {
            switch (tag) {
                .epsilon, .mask => {},
                .either => {
                    try writer.print(" {} -> {}", .{ i, i - 1 });
                    try writer.print(" {} -> {}", .{ i, data.either });
                },
                .concat => {
                    try writer.print(" {} -> {}", .{ i, i - 1 });
                    try writer.print(" {} -> {}", .{ i, data.concat });
                },
                .look_around, .repeat => {
                    try writer.print(" {} -> {}", .{ i, i - 1 });
                },
            }
        }
        try writer.print(" }}", .{});
    }
};
