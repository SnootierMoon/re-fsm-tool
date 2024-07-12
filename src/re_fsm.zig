const std = @import("std");

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
