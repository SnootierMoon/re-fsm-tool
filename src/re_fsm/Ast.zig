const std = @import("std");

const Ast = @This();

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

    pub const Direction = enum { ahead, behind };
    pub const Sign = enum { pos, neg };
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

fn lastNodeStartIdx(nodes: std.MultiArrayList(Node)) usize {
    const slice = nodes.slice();
    const tags = slice.items(.tags);
    const data = slice.items(.data);

    var curr = nodes.len - 1;
    while (true) {
        switch (tags[curr]) {
            .epsilon, .mask => return curr,
            .either => curr = data[curr].either,
            .concat => curr = data[curr].concat,
            .look_around, .repeat => curr -= 1,
        }
    }
}

fn copyNode(gpa: std.mem.Allocator, nodes: *std.MultiArrayList(Node), start: usize, end: usize) error{ Parse, OutOfMemory }!void {
    try nodes.ensureUnusedCapacity(gpa, end - start);
    const slice = nodes.slice();
    const tags = slice.items(.tags);
    const data = slice.items(.data);

    const delta = nodes.len - start;
    for (start..end) |curr| {
        switch (tags[curr]) {
            .epsilon => nodes.appendAssumeCapacity(.{ .epsilon = {} }),
            .mask => nodes.appendAssumeCapacity(.{ .mask = data[curr].mask }),
            .either => nodes.appendAssumeCapacity(.{ .either = data[curr].either + delta }),
            .concat => nodes.appendAssumeCapacity(.{ .concat = data[curr].concat + delta }),
            .look_around => nodes.appendAssumeCapacity(.{ .look_around = data[curr].look_around }),
            .repeat => nodes.appendAssumeCapacity(.{ .repeat = {} }),
        }
    }
}

fn genRepeat(gpa: std.mem.Allocator, nodes: *std.MultiArrayList(Node), low: usize, high: ?usize) error{ Parse, OutOfMemory }!void {
    const start = lastNodeStartIdx(nodes.*);
    const end = nodes.len;
    if (high) |high_| {
        std.debug.assert(low <= high_);
        if (high_ == 0) {
            nodes.len = start;
            try nodes.append(gpa, .{ .epsilon = {} });
        } else if (low == 0) {
            for (1..high_) |_| {
                try nodes.append(gpa, .{ .epsilon = {} });
                try nodes.append(gpa, .{ .either = nodes.len - 2 });
                const tmp_index = nodes.len - 1;
                try copyNode(gpa, nodes, start, end);
                try nodes.append(gpa, .{ .concat = tmp_index });
            }
            try nodes.append(gpa, .{ .epsilon = {} });
            try nodes.append(gpa, .{ .either = nodes.len - 2 });
        } else {
            for (low..high_) |_| {
                try nodes.append(gpa, .{ .epsilon = {} });
                try nodes.append(gpa, .{ .either = nodes.len - 2 });
                const tmp_index = nodes.len - 1;
                try copyNode(gpa, nodes, start, end);
                try nodes.append(gpa, .{ .concat = tmp_index });
            }
            for (1..low) |_| {
                const tmp_index = nodes.len - 1;
                try copyNode(gpa, nodes, start, end);
                try nodes.append(gpa, .{ .concat = tmp_index });
            }
        }
    } else if (low == 0) {
        try nodes.append(gpa, .{ .repeat = {} });
        try nodes.append(gpa, .{ .epsilon = {} });
        try nodes.append(gpa, .{ .either = nodes.len - 2 });
    } else {
        try nodes.append(gpa, .{ .repeat = {} });
        for (1..low) |_| {
            const tmp_index = nodes.len - 1;
            try copyNode(gpa, nodes, start, end);
            try nodes.append(gpa, .{ .concat = tmp_index });
        }
    }
}

const Stream = struct {
    str: []const u8,
    loc: usize = 0,

    fn hasRemaining(s: Stream) bool {
        return s.loc < s.str.len;
    }

    fn peekChar(s: Stream) ?u8 {
        if (s.hasRemaining()) {
            return s.str[s.loc];
        } else {
            return null;
        }
    }

    fn readChar(s: *Stream) ?u8 {
        if (s.hasRemaining()) {
            s.loc += 1;
            return s.str[s.loc - 1];
        } else {
            return null;
        }
    }

    fn readCharIf(s: *Stream, predicate: fn (u8) bool) ?u8 {
        if (s.hasRemaining() and predicate(s.str[s.loc])) {
            s.loc += 1;
            return s.str[s.loc - 1];
        } else {
            return null;
        }
    }

    fn readCharIfEq(s: *Stream, ch: u8) bool {
        if (s.hasRemaining() and s.str[s.loc] == ch) {
            s.loc += 1;
            return true;
        } else {
            return false;
        }
    }

    fn readCharIfNeq(s: *Stream, ch: u8) ?u8 {
        if (s.hasRemaining() and s.str[s.loc] != ch) {
            s.loc += 1;
            return s.str[s.loc - 1];
        } else {
            return null;
        }
    }

    fn readUnsignedInt(s: *Stream, comptime Int: type) error{ Parse, OutOfMemory }!?Int {
        if (s.readCharIf(std.ascii.isDigit)) |ch0| {
            var int: Int = ch0 - '0';
            while (s.readCharIf(std.ascii.isDigit)) |ch| {
                int = std.math.add(Int, std.math.mul(Int, int, 10) catch return error.Parse, ch - '0') catch return error.Parse;
            }
            return int;
        } else {
            return null;
        }
    }

    fn readCharClass(s: *Stream) error{ Parse, OutOfMemory }!u128 {
        var negated = false;
        var last: ?u8 = undefined;
        var class_mask: u128 = 0;
        if (s.readCharIfEq('^')) {
            negated = true;
        }
        if (s.readCharIfEq(']')) {
            last = '-';
            class_mask |= 1 << ']';
        }
        while (s.readCharIfNeq(']')) |ch| {
            if (!std.ascii.isAscii(ch)) {
                return error.Parse;
            }
            if (ch == '-') {
                if (s.readCharIfNeq(']')) |next_ch| {
                    if (last) |last_ch| {
                        class_mask &= ~(@as(u128, 1) << @intCast(last_ch));
                        const lo = @min(last_ch, next_ch);
                        const hi = @max(last_ch, next_ch);
                        class_mask |= (std.math.shl(u128, 1, hi - lo + 1) -% 1) << @intCast(lo);
                        continue;
                    }
                }
            }
            last = ch;
            class_mask |= @as(u128, 1) << @intCast(ch);
        }
        if (!s.readCharIfEq(']')) {
            return error.Parse;
        }
        return if (negated) ~class_mask else class_mask;
    }

    fn readQuantifier(s: *Stream, gpa: std.mem.Allocator, nodes: *std.MultiArrayList(Node)) error{ Parse, OutOfMemory }!void {
        if (s.readCharIfEq('*')) {
            try genRepeat(gpa, nodes, 0, null);
        } else if (s.readCharIfEq('+')) {
            try genRepeat(gpa, nodes, 1, null);
        } else if (s.readCharIfEq('?')) {
            try genRepeat(gpa, nodes, 0, 1);
        } else if (s.readCharIfEq('{')) {
            const low = try s.readUnsignedInt(usize);
            if (s.readCharIfEq('}')) {
                if (low) |low_| {
                    try genRepeat(gpa, nodes, low_, low_);
                } else {
                    return error.Parse;
                }
            } else if (s.readCharIfEq(',')) {
                const high = try s.readUnsignedInt(usize);
                if (s.readCharIfEq('}')) {
                    try genRepeat(gpa, nodes, low orelse 0, high);
                } else {
                    return error.Parse;
                }
            } else {
                return error.Parse;
            }
        }
    }
};

pub fn parse(gpa: std.mem.Allocator, str: []const u8) error{ Parse, OutOfMemory }!Ast {
    const StackFrame = struct {
        term: ?usize = null,
        expr: ?usize = null,
        look_around: ?LookAround = null,

        fn addAtomToTerm(
            frame: *@This(),
            gpa_: std.mem.Allocator,
            nodes: *std.MultiArrayList(Node),
        ) error{OutOfMemory}!void {
            if (frame.term) |atom| {
                try nodes.append(gpa_, .{ .concat = atom });
            }
            frame.term = nodes.len - 1;
        }

        fn addTermToExpr(
            frame: *@This(),
            gpa_: std.mem.Allocator,
            nodes: *std.MultiArrayList(Node),
        ) error{OutOfMemory}!void {
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

    var s: Stream = .{ .str = str };
    var nodes: std.MultiArrayList(Node) = .{};
    errdefer nodes.deinit(gpa);

    var stack = std.ArrayList(StackFrame).init(gpa);
    defer stack.deinit();
    var curr_frame = StackFrame{};

    while (s.hasRemaining()) {
        if (s.readCharIf(isLiteral)) |ch| {
            try nodes.append(gpa, .{ .mask = @as(u128, 1) << @intCast(ch) });
            try s.readQuantifier(gpa, &nodes);
            try curr_frame.addAtomToTerm(gpa, &nodes);
        } else if (s.readCharIfEq('\\')) {
            if (s.readCharIf(isSpecial)) |ch| {
                try nodes.append(gpa, .{ .mask = @as(u128, 1) << @intCast(ch) });
                try s.readQuantifier(gpa, &nodes);
                try curr_frame.addAtomToTerm(gpa, &nodes);
            } else {
                return error.Parse;
            }
        } else if (s.readCharIfEq('.')) {
            try nodes.append(gpa, .{ .mask = std.math.maxInt(u128) });
            try s.readQuantifier(gpa, &nodes);
            try curr_frame.addAtomToTerm(gpa, &nodes);
        } else if (s.readCharIfEq('[')) {
            try nodes.append(gpa, .{ .mask = try s.readCharClass() });
            try s.readQuantifier(gpa, &nodes);
            try curr_frame.addAtomToTerm(gpa, &nodes);
        } else if (s.readCharIfEq('|')) {
            try curr_frame.addTermToExpr(gpa, &nodes);
        } else if (s.readCharIfEq('(')) {
            var look_around: ?LookAround = null;
            if (s.readCharIfEq('?')) {
                look_around = LookAround{ .dir = .ahead, .sign = undefined };
                if (s.readCharIfEq('<')) {
                    look_around.?.dir = .behind;
                }
                if (s.readCharIfEq('!')) {
                    look_around.?.sign = .neg;
                } else if (s.readCharIfEq('=')) {
                    look_around.?.sign = .pos;
                } else {
                    return error.Parse;
                }
            }
            try stack.append(curr_frame);
            curr_frame = StackFrame{ .look_around = look_around };
        } else if (s.readCharIfEq(')')) {
            var old_frame = stack.popOrNull() orelse return error.Parse;
            try curr_frame.addTermToExpr(gpa, &nodes);
            try s.readQuantifier(gpa, &nodes);
            if (curr_frame.look_around) |look_around| {
                try nodes.append(gpa, .{ .look_around = look_around });
            }
            try old_frame.addAtomToTerm(gpa, &nodes);
            curr_frame = old_frame;
        } else {
            return error.Parse;
        }
    }
    try curr_frame.addTermToExpr(gpa, &nodes);

    if (s.hasRemaining() or stack.items.len != 0) {
        return error.Parse;
    }
    return .{ .nodes = nodes };
}

pub fn deinit(ast: *Ast, gpa: std.mem.Allocator) void {
    ast.nodes.deinit(gpa);
}
