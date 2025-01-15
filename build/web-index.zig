//! Walk the root directory served by an http server. A directory containing
//! the files should be passed as the first command line argument.
//! Generate and output Zig code which contains a map from paths to file
//! contents (via @embedFile) that will be initialized at comptime.
//! The generated code can be put in a directory which has the htpp files under
//! "./files", and exposed as a Zig module. The generated code will read the
//! served files into the comptime map with @embedFile, and the map will be
//! exported to other modules.

const std = @import("std");

const header = "pub const map:@import(\"std\").StaticStringMap([]const u8)=.initComptime(.{";

var mem: [32768]u8 = undefined;

pub fn main() !void {
    var fba: std.heap.FixedBufferAllocator = .init(&mem);

    var args = try std.process.argsWithAllocator(fba.allocator());
    defer args.deinit();
    std.debug.assert(args.skip());

    var in_dir: std.fs.Dir = try std.fs.openDirAbsolute(
        args.next().?,
        .{ .iterate = true },
    );
    defer in_dir.close();

    var buffered_writer = std.io.bufferedWriter(std.io.getStdOut().writer());
    const writer = buffered_writer.writer();

    try writer.writeAll(header);

    var walker = try in_dir.walk(fba.allocator());
    defer walker.deinit();
    var arena: std.heap.ArenaAllocator = .init(fba.allocator());
    defer arena.deinit();
    while (try walker.next()) |file_entry| {
        if (file_entry.kind != .file) continue;
        defer _ = arena.reset(.retain_capacity);

        const real_path = try std.fs.path.resolve(arena.allocator(), &.{
            "files",
            file_entry.path,
        });

        var web_path: std.ArrayList(u8) = .init(arena.allocator());
        var it: std.fs.path.NativeComponentIterator = try .init(file_entry.path);
        while (it.next()) |component| {
            try web_path.append('/');
            try web_path.appendSlice(component.name);
        }

        try writer.print(".{{\"{}\",@embedFile(\"{}\")}},", .{
            std.zig.fmtEscapes(web_path.items),
            std.zig.fmtEscapes(real_path),
        });
    }

    try writer.writeAll("});\n");

    try buffered_writer.flush();
}
