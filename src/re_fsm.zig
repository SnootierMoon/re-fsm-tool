const std = @import("std");
const Ast = @import("re_fsm/Ast.zig");
const Nfa = @import("re_fsm/Nfa.zig");
const Dfa = @import("re_fsm/Dfa.zig");

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

pub fn printMask(writer: anytype, mask: u128) !void {
    std.debug.assert(mask != 0);
    if (mask == std.math.maxInt(u128)) {
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

pub fn run(writer: anytype, gpa: std.mem.Allocator, input_regex: []const u8) !void {
    var ast = try Ast.parse(gpa, input_regex);
    defer ast.deinit(gpa);

    // try ast.viz(writer);

    var nfa = try Nfa.init(gpa, ast);
    defer nfa.deinit(gpa);

    var dfa, _ = try Dfa.init(gpa, nfa.groups.items[0]);
    defer dfa.deinit(gpa);

    try dfa.viz(writer);
}
