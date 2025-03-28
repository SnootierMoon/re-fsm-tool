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
        return .{
            .symbol = ast.symbols.len,
            .mask = ast.masks.len,
            .node_tag = ast.node_tags.len,
        };
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

fn check(ast: Ast) bool {
    var symbol_tag_count: usize = 0;
    var mask_tag_count: usize = 0;
    var parts: usize = 0;

    if (ast.node_tags.len == 0) {
        return false;
    }
    for (ast.node_tags) |tag| {
        switch (tag) {
            .symbol => {
                parts += 1;
                symbol_tag_count += 1;
            },
            .mask => {
                parts += 1;
                mask_tag_count += 1;
            },
            .epsilon => parts += 1,
            .either, .concat => if (parts == 0) {
                return false;
            } else {
                parts -= 1;
            },
            .repeat,
            .neg_look_behind,
            .neg_look_ahead,
            .pos_look_behind,
            .pos_look_ahead,
            => {},
        }
        if (parts == 0) {
            return false;
        }
    }
    return parts == 1 and
        // symbol_tag_count == ast.symbols.len and
        mask_tag_count == ast.masks.len;
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

    pub fn next(
        it: *Iterator,
        gpa: std.mem.Allocator,
    ) error{OutOfMemory}!?Node {
        return it.checkedNext(gpa) catch |err| switch (err) {
            error.Invalid => unreachable,
            else => |e| return e,
        };
    }

    pub fn checkedNext(
        it: *Iterator,
        gpa: std.mem.Allocator,
    ) error{ Invalid, OutOfMemory }!?Node {
        if (it.index.node_tag >= it.ast.node_tags.len) {
            if (it.index.symbol != it.ast.symbols.len or
                it.index.mask != it.ast.masks.len or
                it.index.node_tag != it.ast.node_tags.len or
                it.stack.items.len != 0 or
                it.ast.node_tags.len == 0)
            {
                return error.Invalid;
            }
            return null;
        }
        defer it.index.node_tag += 1;
        return switch (it.ast.node_tags[it.index.node_tag]) {
            .symbol => {
                if (it.index.node_tag != 0) {
                    try it.stack.append(gpa, it.index.node_tag - 1);
                }
                return .{ .symbol = it.index.nextSymbol(it.ast) };
            },
            .mask => {
                if (it.index.node_tag != 0) {
                    try it.stack.append(gpa, it.index.node_tag - 1);
                }
                return .{ .mask = it.index.nextMask(it.ast) };
            },
            .epsilon => {
                if (it.index.node_tag != 0) {
                    try it.stack.append(gpa, it.index.node_tag - 1);
                }
                return .epsilon;
            },
            .either => .{ .either = .{
                .lhs = it.stack.pop() orelse return error.Invalid,
                .rhs = it.index.node_tag - 1,
            } },
            .concat => .{ .concat = .{
                .lhs = it.stack.pop() orelse return error.Invalid,
                .rhs = it.index.node_tag - 1,
            } },
            .repeat => {
                if (it.index.node_tag == 0) {
                    return error.Invalid;
                }
                return .{ .repeat = .{
                    .inner = it.index.node_tag - 1,
                } };
            },
            .neg_look_behind => {
                if (it.index.node_tag == 0) {
                    return error.Invalid;
                }
                return .{ .look_around = .{
                    .kind = .{ .sign = .neg, .dir = .behind },
                    .inner = it.index.node_tag - 1,
                } };
            },
            .neg_look_ahead => {
                if (it.index.node_tag == 0) {
                    return error.Invalid;
                }
                return .{ .look_around = .{
                    .kind = .{ .sign = .neg, .dir = .ahead },
                    .inner = it.index.node_tag - 1,
                } };
            },
            .pos_look_behind => {
                if (it.index.node_tag == 0) {
                    return error.Invalid;
                }
                return .{ .look_around = .{
                    .kind = .{ .sign = .pos, .dir = .behind },
                    .inner = it.index.node_tag - 1,
                } };
            },
            .pos_look_ahead => {
                if (it.index.node_tag == 0) {
                    return error.Invalid;
                }
                return .{ .look_around = .{
                    .kind = .{ .sign = .pos, .dir = .ahead },
                    .inner = it.index.node_tag - 1,
                } };
            },
        };
    }
};

test Iterator {
    const test_cases: []const struct { Ast, []const Node } = &.{ .{ .{ .node_tags = &.{ .symbol, .symbol, .concat, .symbol, .concat, .symbol, .concat, .mask, .mask, .mask, .mask, .concat, .concat, .concat, .either }, .symbols = &.{ 'i', 't', 'e', 'r' }, .masks = &.{ 1 << 'i', 1 << 't', 1 << 'e', 1 << 'r' } }, &.{ .{ .symbol = 'i' }, .{ .symbol = 't' }, .{ .concat = .{ .lhs = 0, .rhs = 1 } }, .{ .symbol = 'e' }, .{ .concat = .{ .lhs = 2, .rhs = 3 } }, .{ .symbol = 'r' }, .{ .concat = .{ .lhs = 4, .rhs = 5 } }, .{ .mask = 1 << 'i' }, .{ .mask = 1 << 't' }, .{ .mask = 1 << 'e' }, .{ .mask = 1 << 'r' }, .{ .concat = .{ .lhs = 9, .rhs = 10 } }, .{ .concat = .{ .lhs = 8, .rhs = 11 } }, .{ .concat = .{ .lhs = 7, .rhs = 12 } }, .{ .either = .{ .lhs = 6, .rhs = 13 } } } }, .{ .{ .node_tags = &.{ .epsilon, .repeat, .neg_look_behind, .neg_look_ahead, .pos_look_behind, .pos_look_ahead }, .symbols = &.{}, .masks = &.{} }, &.{ .epsilon, .{ .repeat = .{ .inner = 0 } }, .{ .look_around = .{ .kind = .{ .sign = .neg, .dir = .behind }, .inner = 1 } }, .{ .look_around = .{ .kind = .{ .sign = .neg, .dir = .ahead }, .inner = 2 } }, .{ .look_around = .{ .kind = .{ .sign = .pos, .dir = .behind }, .inner = 3 } }, .{ .look_around = .{ .kind = .{ .sign = .pos, .dir = .ahead }, .inner = 4 } } } } };

    for (test_cases) |test_case| {
        const ast, const node_tags = test_case;
        var it: Ast.Iterator = .init(ast);
        try std.testing.expect(ast.check());
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
        .posix_bre => try parsers.posixBre(gpa, str),
        .posix_ere => try parsers.posixEre(gpa, str),
        .cmsc330 => try parsers.cmsc330(gpa, str),
        .pcre => try parsers.pcre(gpa, str),
    };
}

const fuzz_tests = struct {
    fn genAstFromInput(input: []const u8) anyerror!Ast {
        var symbols: std.ArrayList(u7) = .init(std.testing.allocator);
        defer symbols.deinit();
        var masks: std.ArrayList(u128) = .init(std.testing.allocator);
        defer masks.deinit();
        var node_tags: std.ArrayList(NodeTag) = .init(std.testing.allocator);
        defer node_tags.deinit();
        var i: usize = 0;
        while (i < input.len) : (i += 1) {
            const choices = std.enums.values(NodeTag);
            try node_tags.append(choices[input[i] % choices.len]);
            switch (node_tags.getLast()) {
                .symbol => {
                    try symbols.append(if (i + 1 < input.len) @truncate(input[i + 1]) else 0);
                    i += 1;
                },
                .mask => {
                    var mask_buf: [4]u8 = .{ 0, 0, 0, 0 };
                    std.mem.copyForwards(u8, &mask_buf, input[i + 1 .. @min(input.len, i + 5)]);
                    i += 4;
                    try masks.append(std.mem.readInt(u32, &mask_buf, .little));
                },
                else => {},
            }
        }
        const symbols_arr = try symbols.toOwnedSlice();
        errdefer std.testing.allocator.free(symbols_arr);
        const masks_arr = try masks.toOwnedSlice();
        errdefer std.testing.allocator.free(masks_arr);
        const node_tags_arr = try node_tags.toOwnedSlice();
        errdefer std.testing.allocator.free(node_tags_arr);
        return .{
            .symbols = symbols_arr,
            .masks = masks_arr,
            .node_tags = node_tags_arr,
        };
    }

    fn simpleParse(flavor: Flavor, input: []const u8) anyerror!void {
        var ast = parse(
            std.testing.allocator,
            flavor,
            input,
        ) catch |err| switch (err) {
            error.ParseFail => return,
            error.IllegalChar => return,
            else => return err,
        };
        defer ast.deinit(std.testing.allocator);
        try std.testing.expect(ast.check());
    }

    fn tryIterate(ast: Ast) !void {
        var it: Iterator = .init(ast);
        defer it.deinit(std.testing.allocator);
        while (try it.checkedNext(std.testing.allocator)) |_| {}
    }

    fn validConsistentWithIterable(ctx: void, input: []const u8) anyerror!void {
        _ = ctx;
        var ast = try genAstFromInput(input);
        defer ast.deinit(std.testing.allocator);
        if (ast.check()) {
            try tryIterate(ast);
        } else {
            try std.testing.expectError(error.Invalid, tryIterate(ast));
        }
    }
};

test "fuzz_tests.simpleParse(.posix_bre)" {
    try std.testing.fuzz(Flavor.posix_bre, fuzz_tests.simpleParse, .{});
}

test "fuzz_tests.simpleParse(.posix_ere)" {
    try std.testing.fuzz(Flavor.posix_ere, fuzz_tests.simpleParse, .{});
}

test "fuzz_tests.simpleParse(.cmsc330)" {
    try std.testing.fuzz(Flavor.cmsc330, fuzz_tests.simpleParse, .{});
}

test "fuzz_tests.validConsistentWithIterable" {
    try std.testing.fuzz({}, fuzz_tests.validConsistentWithIterable, .{});
}
