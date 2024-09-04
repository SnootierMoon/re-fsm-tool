const std = @import("std");

pub fn main() !void {
    var gpa: std.heap.GeneralPurposeAllocator(.{}) = .init;
    defer std.debug.assert(gpa.deinit() == .ok);

    var arena: std.heap.ArenaAllocator = .init(gpa.allocator());
    defer arena.deinit();

    var args = try std.process.argsWithAllocator(arena.allocator());
    std.debug.assert(args.skip());

    var files_dir: std.fs.Dir = try std.fs.openDirAbsolute(args.next().?, .{ .iterate = true });
    defer files_dir.close();

    var buffered_writer = std.io.bufferedWriter(std.io.getStdOut().writer());
    const writer = buffered_writer.writer();

    try writer.writeAll("pub const map = @import(\"std\").StaticStringMap([]const u8).initComptime(.{\n");

    var walker = try files_dir.walk(arena.allocator());
    var sub_arena: std.heap.ArenaAllocator = .init(arena.allocator());
    while (try walker.next()) |file_entry| {
        defer _ = sub_arena.reset(.retain_capacity);
        const real_path = try std.fs.path.resolve(sub_arena.allocator(), &.{ "files", file_entry.path });
        var web_path = std.ArrayList([]const u8).init(sub_arena.allocator());
        var it: std.fs.path.NativeComponentIterator = try .init(file_entry.path);
        while (it.next()) |comp| {
            try web_path.append(comp.name);
        }
        try writer.print("    .{{ \"{}\", @embedFile(\"{}\") }},\n", .{
            std.zig.fmtEscapes(try std.fs.path.join(sub_arena.allocator(), web_path.items)),
            std.zig.fmtEscapes(real_path),
        });
    }

    try writer.writeAll("});\n");

    try buffered_writer.flush();
}
