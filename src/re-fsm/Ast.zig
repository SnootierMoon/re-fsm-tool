const std = @import("std");

const Ast = @This();
const Flavor = @import("root.zig").Flavor;
const parsers = @import("Ast/parsers.zig");

symbols: []const u7,
masks: []const u128,
node_tags: []const NodeTag,

pub const NodeTag = enum {
    symbol,
    mask,
    epsilon,
    either,
    concat,
    repeat,
    neg_look_behind,
    neg_look_ahead,
    pos_look_behind,
    pos_look_ahead,

    pub fn fromLookAround(look_around: LookAroundKind) NodeTag {
        return switch (look_around.sign) {
            .neg => switch (look_around.dir) {
                .behind => .neg_look_behind,
                .ahead => .neg_look_ahead,
            },
            .pos => switch (look_around.dir) {
                .behind => .pos_look_behind,
                .ahead => .pos_look_ahead,
            },
        };
    }

    pub fn toLookAround(node_tag: NodeTag) LookAroundKind {
        return switch (node_tag) {
            .neg_look_behind => .{ .sign = .neg, .dir = .behind },
            .neg_look_ahead => .{ .sign = .neg, .dir = .ahead },
            .pos_look_behind => .{ .sign = .pos, .dir = .behind },
            .pos_look_ahead => .{ .sign = .pos, .dir = .ahead },
            else => std.debug.panic("unexpected tag {}", .{node_tag}),
        };
    }
};

pub const LookAroundKind = packed struct(u2) {
    sign: Sign,
    dir: Dir,

    pub const Sign = enum(u1) { neg, pos };
    pub const Dir = enum(u1) { behind, ahead };
};

pub const Index = struct {
    symbol: usize,
    mask: usize,
    node_tag: usize,

    pub const begin: Index = .{
        .symbol = 0,
        .mask = 0,
        .node_tag = 0,
    };

    pub fn end(ast: Ast) Index {
        return .{ .symbol = ast.symbols.len, .mask = ast.masks.len, .node_tag = ast.node_tags.len };
    }

    pub fn nextSymbol(index: *Index, ast: Ast) u7 {
        defer index.symbol += 1;
        return ast.symbols[index.symbol];
    }

    pub fn nextMask(index: *Index, ast: Ast) u128 {
        defer index.mask += 1;
        return ast.masks[index.mask];
    }

    pub fn prevSymbol(index: *Index, ast: Ast) u7 {
        index.symbol -= 1;
        return ast.symbols[index.symbol];
    }

    pub fn prevMask(index: *Index, ast: Ast) u128 {
        index.mask -= 1;
        return ast.masks[index.mask];
    }
};

pub const Node = union(enum) {
    symbol: Symbol,
    mask: Mask,
    epsilon: Epsilon,
    either: Either,
    concat: Concat,
    repeat: Repeat,
    look_around: LookAround,

    pub const Symbol = u7;
    pub const Mask = u128;
    pub const Epsilon = void;
    pub const Either = struct {
        lhs: usize,
        rhs: usize,
    };
    pub const Concat = struct {
        lhs: usize,
        rhs: usize,
    };
    pub const Repeat = struct {
        inner: usize,
    };
    pub const LookAround = struct {
        kind: LookAroundKind,
        inner: usize,
    };
};

pub fn deinit(ast: *Ast, gpa: std.mem.Allocator) void {
    gpa.free(ast.symbols);
    gpa.free(ast.masks);
    gpa.free(ast.node_tags);
}

pub const Iterator = struct {
    ast: Ast,
    stack: std.ArrayListUnmanaged(usize),
    index: Index,

    pub fn init(ast: Ast) Iterator {
        return .{ .ast = ast, .stack = .empty, .index = .begin };
    }

    pub fn deinit(it: *Iterator, gpa: std.mem.Allocator) void {
        it.stack.deinit(gpa);
    }

    pub fn next(it: *Iterator, gpa: std.mem.Allocator) error{OutOfMemory}!?Node {
        if (it.index.node_tag >= it.ast.node_tags.len) {
            std.debug.assert(it.index.symbol == it.ast.symbols.len);
            std.debug.assert(it.index.mask == it.ast.masks.len);
            std.debug.assert(it.index.node_tag == it.ast.node_tags.len);
            std.debug.assert(it.stack.items.len == 0);
            return null;
        }
        defer it.index.node_tag += 1;
        return switch (it.ast.node_tags[it.index.node_tag]) {
            .symbol => {
                if (it.index.node_tag != 0) try it.stack.append(gpa, it.index.node_tag - 1);
                return .{ .symbol = it.index.nextSymbol(it.ast) };
            },
            .mask => {
                if (it.index.node_tag != 0) try it.stack.append(gpa, it.index.node_tag - 1);
                return .{ .mask = it.index.nextMask(it.ast) };
            },
            .epsilon => {
                if (it.index.node_tag != 0) try it.stack.append(gpa, it.index.node_tag - 1);
                return .epsilon;
            },
            .either => .{ .either = .{ .lhs = it.stack.pop(), .rhs = it.index.node_tag - 1 } },
            .concat => .{ .concat = .{ .lhs = it.stack.pop(), .rhs = it.index.node_tag - 1 } },
            .repeat => .{ .repeat = .{ .inner = it.index.node_tag - 1 } },
            .neg_look_behind => .{ .look_around = .{ .kind = .{ .sign = .neg, .dir = .behind }, .inner = it.index.node_tag - 1 } },
            .neg_look_ahead => .{ .look_around = .{ .kind = .{ .sign = .neg, .dir = .ahead }, .inner = it.index.node_tag - 1 } },
            .pos_look_behind => .{ .look_around = .{ .kind = .{ .sign = .pos, .dir = .behind }, .inner = it.index.node_tag - 1 } },
            .pos_look_ahead => .{ .look_around = .{ .kind = .{ .sign = .pos, .dir = .ahead }, .inner = it.index.node_tag - 1 } },
        };
    }
};

test Iterator {
    const test_cases: []const struct { Ast, []const Node } = &.{
        .{
            .{ .node_tags = &.{ .symbol, .symbol, .concat, .symbol, .concat, .symbol, .concat, .mask, .mask, .mask, .mask, .concat, .concat, .concat, .either }, .symbols = &.{ 'i', 't', 'e', 'r' }, .masks = &.{ 1 << 'i', 1 << 't', 1 << 'e', 1 << 'r' } },
            &.{ .{ .symbol = 'i' }, .{ .symbol = 't' }, .{ .concat = .{ .lhs = 0, .rhs = 1 } }, .{ .symbol = 'e' }, .{ .concat = .{ .lhs = 2, .rhs = 3 } }, .{ .symbol = 'r' }, .{ .concat = .{ .lhs = 4, .rhs = 5 } }, .{ .mask = 1 << 'i' }, .{ .mask = 1 << 't' }, .{ .mask = 1 << 'e' }, .{ .mask = 1 << 'r' }, .{ .concat = .{ .lhs = 9, .rhs = 10 } }, .{ .concat = .{ .lhs = 8, .rhs = 11 } }, .{ .concat = .{ .lhs = 7, .rhs = 12 } }, .{ .either = .{ .lhs = 6, .rhs = 13 } } },
        },
        .{
            .{ .node_tags = &.{ .epsilon, .repeat, .neg_look_behind, .neg_look_ahead, .pos_look_behind, .pos_look_ahead }, .symbols = &.{}, .masks = &.{} },
            &.{ .epsilon, .{ .repeat = .{ .inner = 0 } }, .{ .look_around = .{ .kind = .{ .sign = .neg, .dir = .behind }, .inner = 1 } }, .{ .look_around = .{ .kind = .{ .sign = .neg, .dir = .ahead }, .inner = 2 } }, .{ .look_around = .{ .kind = .{ .sign = .pos, .dir = .behind }, .inner = 3 } }, .{ .look_around = .{ .kind = .{ .sign = .pos, .dir = .ahead }, .inner = 4 } } },
        },
    };

    for (test_cases) |test_case| {
        const ast, const node_tags = test_case;
        var it: Ast.Iterator = .init(ast);
        defer it.deinit(std.testing.allocator);
        for (node_tags) |expected_node_tag| {
            const actual_node_tag = try it.next(std.testing.allocator) orelse return error.TestExpectedEqual;
            try std.testing.expectEqual(expected_node_tag, actual_node_tag);
        }
        try std.testing.expectEqual(null, try it.next(std.testing.allocator));
    }
}

pub fn parse(gpa: std.mem.Allocator, flavor: Flavor, str: []const u8) parsers.Error!Ast {
    return switch (flavor) {
        .pcre => try parsers.pcre(gpa, str),
        .posix_bre => try parsers.posixBre(gpa, str),
        .posix_ere => try parsers.posixEre(gpa, str),
    };
}

test "fuzz no panic" {
    const input_bytes = std.testing.fuzzInput(.{});
    var ast = parse(std.testing.allocator, .posix_bre, input_bytes) catch return;
    ast.deinit(std.testing.allocator);
}
