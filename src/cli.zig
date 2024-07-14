const std = @import("std");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const stdin = std.io.getStdIn();
    const stdout = std.io.getStdOut();

    var br = std.io.bufferedReader(stdin.reader());
    var bw = std.io.bufferedWriter(stdout.writer());

    var input = std.ArrayList(u8).init(gpa.allocator());
    defer input.deinit();

    while (true) {
        defer bw.flush() catch {};
        input.clearRetainingCapacity();
        br.reader().streamUntilDelimiter(input.writer(), '\n', 2048) catch |err| switch (err) {
            error.EndOfStream, error.StreamTooLong => break,
            else => |e| return e,
        };

        @import("re_fsm.zig").run(bw.writer(), gpa.allocator(), input.items) catch |err| switch (err) {
            error.Parse => {
                try bw.writer().print("parse error\n", .{});
                continue;
            },
            else => |e| return e,
        };
        try bw.writer().writeByte('\n');
    }
}
