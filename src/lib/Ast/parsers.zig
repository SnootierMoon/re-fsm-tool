const std = @import("std");

const Ast = @import("../Ast.zig");
const ascii = @import("../ascii.zig");

pub const Error = error{ OutOfMemory, ParseFail, IllegalChar };

const RepetitionModifier = struct {
    low: usize,
    high: ?usize,
};

const Stream = struct {
    str: []const u8,
    pos: usize,

    fn init(str: []const u8) error{IllegalChar}!Stream {
        for (str) |byte| {
            if (byte == 0 or byte >= 128) {
                return error.IllegalChar;
            }
        }
        return .{ .str = str, .pos = 0 };
    }

    fn hasRemaining(s: Stream) bool {
        return s.pos < s.str.len;
    }

    fn peekChar(s: Stream) ?u7 {
        if (s.hasRemaining()) {
            return @intCast(s.str[s.pos]);
        } else {
            return null;
        }
    }

    fn readChar(s: *Stream) ?u7 {
        if (s.hasRemaining()) {
            defer s.pos += 1;
            return @intCast(s.str[s.pos]);
        } else {
            return null;
        }
    }

    fn readString(s: *Stream, str: []const u8) bool {
        if (s.pos + str.len <= s.str.len and std.mem.eql(u8, s.str[s.pos..][0..str.len], str)) {
            s.pos += str.len;
            return true;
        } else {
            return false;
        }
    }

    fn readCharIf(s: *Stream, predicate: fn (u7) bool) ?u7 {
        if (s.hasRemaining() and predicate(@intCast(s.str[s.pos]))) {
            defer s.pos += 1;
            return @intCast(s.str[s.pos]);
        } else {
            return null;
        }
    }

    fn readCharIfEq(s: *Stream, ch: u7) bool {
        if (s.hasRemaining() and s.str[s.pos] == ch) {
            s.pos += 1;
            return true;
        } else {
            return false;
        }
    }

    fn readCharIfNeq(s: *Stream, ch: u7) ?u7 {
        if (s.hasRemaining() and s.str[s.pos] != ch) {
            defer s.pos += 1;
            return @intCast(s.str[s.pos]);
        } else {
            return null;
        }
    }

    fn readUnsignedInt(s: *Stream, comptime Int: type) error{ParseFail}!?Int {
        if (s.readCharIf(ascii.Class.digit.cond())) |ch0| {
            var int: Int = ch0 - '0';
            while (s.readCharIf(ascii.Class.digit.cond())) |ch| {
                int = std.math.add(Int, std.math.mul(Int, int, 10) catch return error.ParseFail, ch - '0') catch return error.ParseFail;
            }
            return int;
        } else {
            return null;
        }
    }
};

const Builder = struct {
    symbols: std.ArrayListUnmanaged(u7),
    masks: std.ArrayListUnmanaged(u128),
    node_tags: std.ArrayListUnmanaged(Ast.NodeTag),
    stack_frames: std.ArrayListUnmanaged(StackFrame),
    curr_frame: StackFrame,

    pub const init: Builder = .{
        .symbols = .empty,
        .masks = .empty,
        .node_tags = .empty,
        .stack_frames = .empty,
        .curr_frame = .init,
    };

    const StackFrame = struct {
        term_atoms: usize,
        expr_terms: usize,
        look_around: ?Ast.LookAroundKind,

        pub const init: StackFrame = .{
            .term_atoms = 0,
            .expr_terms = 0,
            .look_around = null,
        };
    };

    fn deinit(bld: *Builder, gpa: std.mem.Allocator) void {
        bld.symbols.deinit(gpa);
        bld.masks.deinit(gpa);
        bld.node_tags.deinit(gpa);
        bld.stack_frames.deinit(gpa);
    }

    fn lastNodeEndIndex(bld: Builder) Ast.Index {
        return .{ .symbol = bld.symbols.items.len, .mask = bld.masks.items.len, .node_tag = bld.node_tags.items.len };
    }

    fn lastNodeBeginIndex(bld: Builder) Ast.Index {
        var curr = bld.lastNodeEndIndex();
        var skip: usize = 0;
        while (true) {
            curr.node_tag = curr.node_tag - 1;
            switch (bld.node_tags.items[curr.node_tag]) {
                .symbol, .mask, .epsilon => {
                    switch (bld.node_tags.items[curr.node_tag]) {
                        .symbol => curr.symbol = curr.symbol - 1,
                        .mask => curr.mask = curr.mask - 1,
                        else => {},
                    }
                    if (skip == 0) {
                        return curr;
                    } else {
                        skip = skip - 1;
                    }
                },
                .concat, .either => skip = skip + 1,
                else => {},
            }
        }
    }

    fn pushSymbol(bld: *Builder, gpa: std.mem.Allocator, symbol: u7) Error!void {
        try bld.symbols.append(gpa, symbol);
        try bld.node_tags.append(gpa, .symbol);
    }

    fn pushMask(bld: *Builder, gpa: std.mem.Allocator, mask: u128) Error!void {
        if (@popCount(mask) == 1) {
            try bld.pushSymbol(gpa, @intCast(@ctz(mask)));
        } else {
            try bld.masks.append(gpa, mask);
            try bld.node_tags.append(gpa, .mask);
        }
    }

    fn pushBeginAnchor(bld: *Builder, gpa: std.mem.Allocator) Error!void {
        try bld.masks.append(gpa, std.math.maxInt(u128));
        try bld.node_tags.append(gpa, .mask);
        try bld.node_tags.append(gpa, .repeat);
        try bld.node_tags.append(gpa, .neg_look_behind);
    }

    fn pushEndAnchor(bld: *Builder, gpa: std.mem.Allocator) Error!void {
        try bld.masks.append(gpa, std.math.maxInt(u128));
        try bld.node_tags.append(gpa, .mask);
        try bld.node_tags.append(gpa, .repeat);
        try bld.node_tags.append(gpa, .neg_look_ahead);
    }

    fn pushRepetitionModifier(bld: *Builder, gpa: std.mem.Allocator, rep: RepetitionModifier) Error!void {
        const begin = bld.lastNodeBeginIndex();
        const end = bld.lastNodeEndIndex();
        if (rep.high) |high| {
            std.debug.assert(rep.low <= high);
            if (high == 0) {
                bld.symbols.shrinkRetainingCapacity(begin.symbol);
                bld.masks.shrinkRetainingCapacity(begin.mask);
                bld.node_tags.shrinkRetainingCapacity(begin.node_tag);
                try bld.node_tags.append(gpa, .epsilon);
            } else if (rep.low == 0) {
                for (1..high) |_| {
                    try bld.node_tags.append(gpa, .epsilon);
                    try bld.node_tags.append(gpa, .either);
                    try bld.pushCopyOfNode(gpa, begin, end);
                    try bld.node_tags.append(gpa, .concat);
                }
                try bld.node_tags.append(gpa, .epsilon);
                try bld.node_tags.append(gpa, .either);
            } else {
                for (rep.low..high) |_| {
                    try bld.node_tags.append(gpa, .epsilon);
                    try bld.node_tags.append(gpa, .either);
                    try bld.pushCopyOfNode(gpa, begin, end);
                    try bld.node_tags.append(gpa, .concat);
                }
                for (1..rep.low) |_| {
                    try bld.pushCopyOfNode(gpa, begin, end);
                    try bld.node_tags.append(gpa, .concat);
                }
            }
        } else if (rep.low == 0) {
            try bld.node_tags.append(gpa, .repeat);
            try bld.node_tags.append(gpa, .epsilon);
            try bld.node_tags.append(gpa, .either);
        } else {
            try bld.node_tags.append(gpa, .repeat);
            for (1..rep.low) |_| {
                try bld.pushCopyOfNode(gpa, begin, end);
                try bld.node_tags.append(gpa, .concat);
            }
        }
    }

    fn pushCopyOfNode(bld: *Builder, gpa: std.mem.Allocator, begin: Ast.Index, end: Ast.Index) Error!void {
        var curr = begin;
        while (curr.node_tag < end.node_tag) : (curr.node_tag += 1) {
            switch (bld.node_tags.items[curr.node_tag]) {
                .symbol => {
                    defer curr.symbol += 1;
                    try bld.symbols.append(gpa, bld.symbols.items[curr.symbol]);
                },
                .mask => {
                    defer curr.mask += 1;
                    try bld.masks.append(gpa, bld.masks.items[curr.mask]);
                },
                else => {},
            }
            try bld.node_tags.append(gpa, bld.node_tags.items[curr.node_tag]);
        }
        std.debug.assert(curr.symbol == end.symbol);
        std.debug.assert(curr.mask == end.mask);
        std.debug.assert(curr.node_tag == end.node_tag);
    }

    fn endAtom(bld: *Builder, gpa: std.mem.Allocator) Error!void {
        _ = gpa;
        bld.curr_frame.term_atoms += 1;
    }

    fn endTerm(bld: *Builder, gpa: std.mem.Allocator) Error!void {
        if (bld.curr_frame.term_atoms == 0) {
            try bld.node_tags.append(gpa, .epsilon);
        } else {
            for (1..bld.curr_frame.term_atoms) |_| {
                try bld.node_tags.append(gpa, .concat);
            }
        }

        bld.curr_frame.term_atoms = 0;
        bld.curr_frame.expr_terms += 1;
    }

    fn endExpr(bld: *Builder, gpa: std.mem.Allocator) Error!void {
        if (bld.curr_frame.expr_terms == 0) {
            try bld.masks.append(gpa, 0);
            try bld.node_tags.append(gpa, .mask);
        } else {
            for (1..bld.curr_frame.expr_terms) |_| {
                try bld.node_tags.append(gpa, .either);
            }
        }
        bld.curr_frame.expr_terms = 0;
    }

    fn finish(bld: *Builder, gpa: std.mem.Allocator) Error!Ast {
        const symbols = try bld.symbols.toOwnedSlice(gpa);
        errdefer gpa.free(symbols);
        const masks = try bld.masks.toOwnedSlice(gpa);
        errdefer gpa.free(masks);
        const node_tags = try bld.node_tags.toOwnedSlice(gpa);
        errdefer gpa.free(node_tags);
        if (bld.stack_frames.items.len != 0) {
            return error.ParseFail;
        }
        return .{ .symbols = symbols, .masks = masks, .node_tags = node_tags };
    }
};

/// Read a bracket expression.
///
/// The initial "[" should already be taken from the stream. This
/// function will take the terminating "]" from the stream.
///
/// See POSIX.1-2024, Section 9.3.5.
fn posixBracketExpression(s: *Stream) Error!u128 {
    const begin_index = s.pos;
    const has_circumflex = s.readCharIfEq('^');
    var mask: u128 = 0;
    var prev_ch: ?u7 = null;

    if (s.readCharIfEq(']')) {
        prev_ch = ']';
        mask |= 1 << ']';
    } else if (s.readCharIfEq('-')) {
        prev_ch = '-';
        mask |= 1 << '-';
    }

    while (s.readCharIfNeq(']')) |ch| {
        if (ch == '-') {
            const low_ch = prev_ch orelse return error.ParseFail;
            if (s.readCharIfNeq(']')) |next_ch| {
                const high_ch = high: {
                    if (next_ch == '[') {
                        if (s.readCharIfEq('.')) {
                            const collating_ch = s.readCharIf(ascii.Class.print.cond()) orelse return error.ParseFail;
                            if (!s.readCharIfEq('.') or !s.readCharIfEq(']')) {
                                return error.ParseFail;
                            }
                            break :high collating_ch;
                        }
                    }
                    if (!ascii.Class.print.contains(next_ch)) {
                        return error.ParseFail;
                    }
                    break :high next_ch;
                };
                if (high_ch < low_ch) {
                    return error.ParseFail;
                }
                mask |= (std.math.shl(u128, 1, high_ch - low_ch + 1) -% 1) << low_ch;
                prev_ch = null;
                continue;
            }
        }
        if (ch == '[') {
            if (s.readCharIfEq('.')) {
                const collating_ch = s.readCharIf(ascii.Class.print.cond()) orelse return error.ParseFail;
                prev_ch = collating_ch;
                mask |= @as(u128, 1) << collating_ch;
                if (!s.readCharIfEq('.') or !s.readCharIfEq(']')) {
                    return error.ParseFail;
                }
                continue;
            } else if (s.readCharIfEq('=')) {
                const equiv_class_ch = s.readCharIf(ascii.Class.print.cond()) orelse return error.ParseFail;
                prev_ch = null;
                mask |= @as(u128, 1) << equiv_class_ch;
                if (!s.readCharIfEq('=') or !s.readCharIfEq(']')) {
                    return error.ParseFail;
                }
                continue;
            } else if (s.readCharIfEq(':')) {
                const class_mask = for (std.enums.values(ascii.Class)) |class| {
                    if (s.readString(@tagName(class))) {
                        break class.mask();
                    }
                } else return error.ParseFail;
                mask |= class_mask;
                prev_ch = null;
                if (!s.readCharIfEq(':') or !s.readCharIfEq(']')) {
                    return error.ParseFail;
                }
                continue;
            }
        }
        if (ascii.Class.print.contains(ch)) {
            prev_ch = ch;
            mask |= @as(u128, 1) << ch;
        } else {
            return error.ParseFail;
        }
    }

    const end_index = s.pos - 1;
    if (!s.readCharIfEq(']')) {
        return error.ParseFail;
    }
    // Section 9.3.5, item 8
    if (end_index - begin_index >= 2 and (s.str[begin_index] == '.' or s.str[begin_index] == '=' or s.str[begin_index] == ':') and s.str[begin_index] == s.str[end_index]) {}
    return if (has_circumflex) ~mask else mask;
}

fn posixIntervalExpression(s: *Stream, mode: enum { bre, ere }) Error!RepetitionModifier {
    const low = try s.readUnsignedInt(usize) orelse return error.ParseFail;
    const rep: RepetitionModifier = if (s.readCharIfEq(','))
        .{ .low = low, .high = try s.readUnsignedInt(usize) }
    else
        .{ .low = low, .high = low };
    if ((mode == .bre and !s.readCharIfEq('\\')) or !s.readCharIfEq('}')) {
        return error.ParseFail;
    }
    return rep;
}

/// Parse a POSIX BRE.
///
/// See POSIX.1-2024, Section 9.3.
pub fn posixBre(gpa: std.mem.Allocator, str: []const u8) Error!Ast {
    var state: enum {
        beginning_of_term,
        after_initial_circumflex,
        after_atom,
        after_repetition_modifier,
        after_dollar,
        end_of_term,
    } = .beginning_of_term;
    var bld: Builder = .init;
    defer bld.deinit(gpa);

    var s = try Stream.init(str);

    while (s.readChar()) |ch| {
        switch (ch) {
            '$' => {
                switch (state) {
                    .beginning_of_term,
                    .after_initial_circumflex,
                    .after_repetition_modifier,
                    => {},
                    .after_atom => try bld.endAtom(gpa),
                    .after_dollar => {
                        try bld.pushSymbol(gpa, '$');
                        try bld.endAtom(gpa);
                    },
                    .end_of_term => return error.ParseFail,
                }
                state = .after_dollar;
            },
            '*' => {
                switch (state) {
                    .beginning_of_term,
                    .after_initial_circumflex,
                    => {
                        try bld.pushSymbol(gpa, '*');
                        state = .after_atom;
                        continue;
                    },
                    .after_atom => {},
                    .after_repetition_modifier,
                    .end_of_term,
                    => return error.ParseFail,
                    .after_dollar => try bld.pushSymbol(gpa, '$'),
                }
                try bld.pushRepetitionModifier(gpa, .{ .low = 0, .high = null });
                try bld.endAtom(gpa);
                state = .after_repetition_modifier;
            },
            '.' => {
                switch (state) {
                    .beginning_of_term,
                    .after_initial_circumflex,
                    .after_repetition_modifier,
                    => {},
                    .after_atom => try bld.endAtom(gpa),
                    .after_dollar => {
                        try bld.pushSymbol(gpa, '$');
                        try bld.endAtom(gpa);
                    },
                    .end_of_term => return error.ParseFail,
                }
                try bld.pushMask(gpa, std.math.maxInt(u128));
                state = .after_atom;
            },
            '[' => {
                switch (state) {
                    .beginning_of_term,
                    .after_initial_circumflex,
                    .after_repetition_modifier,
                    => {},
                    .after_atom => try bld.endAtom(gpa),
                    .after_dollar => {
                        try bld.pushSymbol(gpa, '$');
                        try bld.endAtom(gpa);
                    },
                    .end_of_term => return error.ParseFail,
                }
                const mask = try posixBracketExpression(&s);
                try bld.pushMask(gpa, mask);
                state = .after_atom;
            },
            '\\' => switch (s.readChar() orelse return error.ParseFail) {
                '$' => {
                    switch (state) {
                        .beginning_of_term,
                        .after_initial_circumflex,
                        .after_repetition_modifier,
                        => {},
                        .after_atom => try bld.endAtom(gpa),
                        .after_dollar => {
                            try bld.pushSymbol(gpa, '$');
                            try bld.endAtom(gpa);
                        },
                        .end_of_term => return error.ParseFail,
                    }
                    try bld.pushSymbol(gpa, '$');
                    state = .end_of_term;
                },
                '(' => {
                    switch (state) {
                        .beginning_of_term,
                        .after_initial_circumflex,
                        .after_repetition_modifier,
                        => {},
                        .after_atom => try bld.endAtom(gpa),
                        .after_dollar => {
                            try bld.pushSymbol(gpa, '$');
                            try bld.endAtom(gpa);
                        },
                        .end_of_term => return error.ParseFail,
                    }
                    try bld.stack_frames.append(gpa, bld.curr_frame);
                    bld.curr_frame = .init;
                    state = .beginning_of_term;
                },
                ')' => {
                    switch (state) {
                        .beginning_of_term => return error.ParseFail,
                        .after_initial_circumflex,
                        .after_repetition_modifier,
                        .end_of_term,
                        => {},
                        .after_atom => try bld.endAtom(gpa),
                        .after_dollar => {
                            try bld.pushEndAnchor(gpa);
                            try bld.endAtom(gpa);
                        },
                    }
                    try bld.endTerm(gpa);
                    try bld.endExpr(gpa);
                    bld.curr_frame = bld.stack_frames.popOrNull() orelse return error.ParseFail;
                    state = .after_atom;
                },
                '*' => {
                    switch (state) {
                        .beginning_of_term,
                        .after_initial_circumflex,
                        .end_of_term,
                        => return error.ParseFail,
                        .after_atom => try bld.endAtom(gpa),
                        .after_repetition_modifier => {},
                        .after_dollar => try bld.pushSymbol(gpa, '$'),
                    }
                    try bld.pushSymbol(gpa, '*');
                    state = .after_atom;
                },
                '+' => {
                    switch (state) {
                        .beginning_of_term,
                        .after_initial_circumflex,
                        .after_repetition_modifier,
                        .end_of_term,
                        => return error.ParseFail,
                        .after_atom => {},
                        .after_dollar => try bld.pushSymbol(gpa, '$'),
                    }
                    try bld.pushRepetitionModifier(gpa, .{ .low = 1, .high = null });
                    try bld.endAtom(gpa);
                    state = .after_repetition_modifier;
                },
                '.', '[', '\\', ']' => |ch_esc| {
                    switch (state) {
                        .beginning_of_term,
                        .after_initial_circumflex,
                        .after_repetition_modifier,
                        => {},
                        .after_atom => try bld.endAtom(gpa),
                        .after_dollar => {
                            try bld.pushSymbol(gpa, '$');
                            try bld.endAtom(gpa);
                        },
                        .end_of_term => return error.ParseFail,
                    }
                    try bld.pushSymbol(gpa, ch_esc);
                    state = .after_atom;
                },
                '?' => {
                    switch (state) {
                        .beginning_of_term,
                        .after_initial_circumflex,
                        .after_repetition_modifier,
                        .end_of_term,
                        => return error.ParseFail,
                        .after_atom => {},
                        .after_dollar => try bld.pushSymbol(gpa, '$'),
                    }
                    try bld.pushRepetitionModifier(gpa, .{ .low = 0, .high = 1 });
                    try bld.endAtom(gpa);
                    state = .after_repetition_modifier;
                },
                '^' => {
                    switch (state) {
                        .beginning_of_term => {},
                        .after_initial_circumflex,
                        .after_atom,
                        .after_repetition_modifier,
                        .after_dollar,
                        .end_of_term,
                        => return error.ParseFail,
                    }
                    try bld.pushSymbol(gpa, '^');
                    state = .after_atom;
                },
                '{' => {
                    switch (state) {
                        .beginning_of_term,
                        .after_initial_circumflex,
                        .after_repetition_modifier,
                        .end_of_term,
                        => return error.ParseFail,
                        .after_atom => {},
                        .after_dollar => try bld.pushSymbol(gpa, '$'),
                    }
                    const rep = try posixIntervalExpression(&s, .bre);
                    try bld.pushRepetitionModifier(gpa, rep);
                    try bld.endAtom(gpa);
                    state = .after_repetition_modifier;
                },
                '|' => {
                    switch (state) {
                        .beginning_of_term => return error.ParseFail,
                        .after_initial_circumflex,
                        .after_repetition_modifier,
                        .end_of_term,
                        => {},
                        .after_atom => try bld.endAtom(gpa),
                        .after_dollar => {
                            try bld.pushEndAnchor(gpa);
                            try bld.endAtom(gpa);
                        },
                    }
                    try bld.endTerm(gpa);
                    state = .beginning_of_term;
                },
                else => return error.ParseFail,
            },
            '^' => {
                switch (state) {
                    .beginning_of_term => {
                        if (bld.stack_frames.items.len > 0) {
                            return error.ParseFail;
                        }
                        try bld.pushBeginAnchor(gpa);
                        try bld.endAtom(gpa);
                        state = .after_initial_circumflex;
                        continue;
                    },
                    .after_initial_circumflex,
                    .after_atom,
                    .after_repetition_modifier,
                    => {},
                    .after_dollar => {
                        try bld.pushSymbol(gpa, '$');
                        try bld.endAtom(gpa);
                    },
                    .end_of_term => return error.ParseFail,
                }
                try bld.pushSymbol(gpa, '^');
                state = .after_atom;
            },
            else => {
                switch (state) {
                    .beginning_of_term,
                    .after_initial_circumflex,
                    .after_repetition_modifier,
                    => {},
                    .after_atom => try bld.endAtom(gpa),
                    .after_dollar => {
                        try bld.pushSymbol(gpa, '$');
                        try bld.endAtom(gpa);
                    },
                    .end_of_term => return error.ParseFail,
                }
                try bld.pushSymbol(gpa, ch);
                state = .after_atom;
            },
        }
    }

    switch (state) {
        .beginning_of_term => return error.ParseFail,
        .after_initial_circumflex,
        .after_repetition_modifier,
        .end_of_term,
        => {},
        .after_atom => try bld.endAtom(gpa),
        .after_dollar => {
            try bld.pushEndAnchor(gpa);
            try bld.endAtom(gpa);
        },
    }
    try bld.endTerm(gpa);
    try bld.endExpr(gpa);
    return bld.finish(gpa);
}

/// Parse a POSIX ERE.
///
/// See POSIX.1-2024, Section 9.4.
pub fn posixEre(gpa: std.mem.Allocator, str: []const u8) Error!Ast {
    var state: enum {
        beginning_of_term,
        after_atom,
        not_after_atom,
    } = .beginning_of_term;
    var bld: Builder = .init;
    defer bld.deinit(gpa);

    var s = try Stream.init(str);

    while (s.readChar()) |ch| {
        switch (ch) {
            '$' => {
                switch (state) {
                    .beginning_of_term,
                    .not_after_atom,
                    => {},
                    .after_atom => try bld.endAtom(gpa),
                }
                try bld.pushEndAnchor(gpa);
                try bld.endAtom(gpa);
                state = .not_after_atom;
            },
            '(' => {
                switch (state) {
                    .beginning_of_term,
                    .not_after_atom,
                    => {},
                    .after_atom => try bld.endAtom(gpa),
                }
                try bld.stack_frames.append(gpa, bld.curr_frame);
                bld.curr_frame = .init;
                state = .beginning_of_term;
            },
            ')' => {
                switch (state) {
                    .beginning_of_term => return error.ParseFail,
                    .after_atom => try bld.endAtom(gpa),
                    .not_after_atom => {},
                }
                try bld.endTerm(gpa);
                try bld.endExpr(gpa);
                bld.curr_frame = bld.stack_frames.popOrNull() orelse return error.ParseFail;
                state = .after_atom;
            },
            '*' => {
                switch (state) {
                    .beginning_of_term,
                    .not_after_atom,
                    => return error.ParseFail,
                    .after_atom => {},
                }
                _ = s.readCharIfEq('?');
                try bld.pushRepetitionModifier(gpa, .{ .low = 0, .high = null });
                try bld.endAtom(gpa);
                state = .not_after_atom;
            },
            '+' => {
                switch (state) {
                    .beginning_of_term,
                    .not_after_atom,
                    => return error.ParseFail,
                    .after_atom => {},
                }
                _ = s.readCharIfEq('?');
                try bld.pushRepetitionModifier(gpa, .{ .low = 1, .high = null });
                try bld.endAtom(gpa);
                state = .not_after_atom;
            },
            '.' => {
                switch (state) {
                    .beginning_of_term,
                    .not_after_atom,
                    => {},
                    .after_atom => try bld.endAtom(gpa),
                }
                try bld.pushMask(gpa, std.math.maxInt(u128));
                state = .after_atom;
            },
            '?' => {
                switch (state) {
                    .beginning_of_term,
                    .not_after_atom,
                    => return error.ParseFail,
                    .after_atom => {},
                }
                _ = s.readCharIfEq('?');
                try bld.pushRepetitionModifier(gpa, .{ .low = 0, .high = 1 });
                try bld.endAtom(gpa);
                state = .not_after_atom;
            },
            '[' => {
                switch (state) {
                    .beginning_of_term,
                    .not_after_atom,
                    => {},
                    .after_atom => try bld.endAtom(gpa),
                }
                const mask = try posixBracketExpression(&s);
                try bld.pushMask(gpa, mask);
                state = .after_atom;
            },
            '\\' => switch (s.readChar() orelse return error.ParseFail) {
                '$', '(', ')', '*', '+', '.', '?', '[', '\\', ']', '^', '{', '|', '}' => |ch_esc| {
                    switch (state) {
                        .beginning_of_term,
                        .not_after_atom,
                        => {},
                        .after_atom => try bld.endAtom(gpa),
                    }
                    try bld.pushSymbol(gpa, ch_esc);
                    state = .after_atom;
                },
                else => return error.ParseFail,
            },
            '^' => {
                switch (state) {
                    .beginning_of_term,
                    .not_after_atom,
                    => {},
                    .after_atom => try bld.endAtom(gpa),
                }
                try bld.pushBeginAnchor(gpa);
                try bld.endAtom(gpa);
                state = .not_after_atom;
            },
            '{' => {
                switch (state) {
                    .beginning_of_term,
                    .not_after_atom,
                    => return error.ParseFail,
                    .after_atom => {},
                }
                const rep = try posixIntervalExpression(&s, .ere);
                _ = s.readCharIfEq('?');
                try bld.pushRepetitionModifier(gpa, rep);
                try bld.endAtom(gpa);
                state = .not_after_atom;
            },
            '|' => {
                switch (state) {
                    .beginning_of_term => return error.ParseFail,
                    .after_atom => try bld.endAtom(gpa),
                    .not_after_atom => {},
                }
                try bld.endTerm(gpa);
                state = .beginning_of_term;
            },
            else => {
                switch (state) {
                    .beginning_of_term,
                    .not_after_atom,
                    => {},
                    .after_atom => try bld.endAtom(gpa),
                }
                try bld.pushSymbol(gpa, ch);
                state = .after_atom;
            },
        }
    }

    switch (state) {
        .beginning_of_term => return error.ParseFail,
        .after_atom => try bld.endAtom(gpa),
        .not_after_atom => {},
    }
    try bld.endTerm(gpa);
    try bld.endExpr(gpa);
    return bld.finish(gpa);
}

/// Parse a CMSC330 Regex.
pub fn cmsc330(gpa: std.mem.Allocator, str: []const u8) Error!Ast {
    var state: enum {
        beginning_of_term,
        after_atom,
        not_after_atom,
    } = .beginning_of_term;
    var bld: Builder = .init;
    defer bld.deinit(gpa);

    var s = try Stream.init(str);

    while (s.readChar()) |ch| {
        switch (ch) {
            '(' => {
                switch (state) {
                    .beginning_of_term,
                    .not_after_atom,
                    => {},
                    .after_atom => try bld.endAtom(gpa),
                }
                try bld.stack_frames.append(gpa, bld.curr_frame);
                bld.curr_frame = .init;
                state = .beginning_of_term;
            },
            ')' => {
                switch (state) {
                    .beginning_of_term,
                    .not_after_atom => {},
                    .after_atom => try bld.endAtom(gpa),
                }
                try bld.endTerm(gpa);
                try bld.endExpr(gpa);
                bld.curr_frame = bld.stack_frames.popOrNull() orelse return error.ParseFail;
                state = .after_atom;
            },
            '*' => {
                switch (state) {
                    .beginning_of_term,
                    .not_after_atom,
                    => return error.ParseFail,
                    .after_atom => {},
                }
                try bld.pushRepetitionModifier(gpa, .{ .low = 0, .high = null });
                try bld.endAtom(gpa);
                state = .not_after_atom;
            },
            '|' => {
                switch (state) {
                    .beginning_of_term,
                    .not_after_atom => {},
                    .after_atom => try bld.endAtom(gpa),
                }
                try bld.endTerm(gpa);
                state = .beginning_of_term;
            },
            'a'...'z', 'A'...'Z' => {
                switch (state) {
                    .beginning_of_term,
                    .not_after_atom,
                    => {},
                    .after_atom => try bld.endAtom(gpa),
                }
                try bld.pushSymbol(gpa, ch);
                state = .after_atom;
            },
            else => return error.ParseFail,
        }
    }

    switch (state) {
        .beginning_of_term,
        .not_after_atom => {},
        .after_atom => try bld.endAtom(gpa),
    }
    try bld.endTerm(gpa);
    try bld.endExpr(gpa);
    return bld.finish(gpa);
}

/// Parse a PCRE.
pub fn pcre(gpa: std.mem.Allocator, str: []const u8) Error!Ast {
    _ = .{ gpa, str };
    unreachable;
}
