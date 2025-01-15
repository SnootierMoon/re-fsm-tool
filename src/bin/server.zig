//! Barebones http server.

const std = @import("std");

pub const options: std.Options = .{
    .log_level = switch (std.builtin.mode) {
        .Debug => .debug,
        else => .info,
    },
};

const web_file_map = @import("web").map;

var mem: [8096]u8 = undefined;

const html_404 =
    \\<!DOCTYPE html>
    \\<html lang="en">
    \\    <head>
    \\        <meta charset="UTF-8">
    \\        <meta name="viewport" content="width=device-width, initial-scale=1.0">
    \\        <title>Regex FSM</title>
    \\    </head>
    \\    <body>
    \\        <h1>404 not found</h1>
    \\        <a href="/">return to home</a>
    \\    </body>
    \\</html>
;

pub fn main() !void {
    var fba: std.heap.FixedBufferAllocator = .init(&mem);
    var args: std.process.ArgIterator = try .initWithAllocator(fba.allocator());
    var address: std.net.Address = .initIp4(.{ 0, 0, 0, 0 }, 8080);
    var open: bool = false;
    std.debug.assert(args.skip());
    while (args.next()) |opt| {
        if (std.mem.eql(u8, "-port", opt)) {
            const port_str = args.next() orelse return error.MissingArgument;
            const port = try std.fmt.parseUnsigned(u16, port_str, 10);
            address.setPort(port);
        } else if (std.mem.eql(u8, "-addr", opt)) {
            const addr_str = args.next() orelse return error.MissingArgument;
            address = try .parseIp(addr_str, address.getPort());
        } else if (std.mem.eql(u8, "-open", opt)) {
            open = true;
        } else {
            return error.InvalidArgument;
        }
    }

    fba.reset();
    const url = try std.fmt.allocPrint(fba.allocator(), "http://{}", .{address});

    std.log.info("listening on {s}", .{url});
    var server = try address.listen(.{ .reuse_address = true });
    defer server.deinit();

    if (open) {
        std.log.info("opening {s} in the browser...", .{url});
        const main_exe = switch (@import("builtin").os.tag) {
            .windows => "explorer",
            .macos => "open",
            else => "xdg-open",
        };
        var child: std.process.Child = .init(&.{ main_exe, url }, fba.allocator());
        child.stdin_behavior = .Ignore;
        child.stdout_behavior = .Ignore;
        child.stderr_behavior = .Ignore;
        try child.spawn();
    }

    while (true) {
        const connection = try server.accept();
        defer connection.stream.close();
        handleConnection(connection) catch |err| {
            std.log.err("{s}", .{@errorName(err)});
        };
    }
}

fn handleConnection(connection: std.net.Server.Connection) !void {
    var http: std.http.Server = .init(connection, &mem);
    var request = http.receiveHead() catch |err| switch (err) {
        error.HttpConnectionClosing => return,
        else => return err,
    };

    const target = if (std.mem.eql(u8, request.head.target, "/"))
        "/index.html"
    else
        request.head.target;

    std.log.info("serving {s}", .{target});

    if (web_file_map.get(target)) |file_contents| {
        const content_type = determineContentType(target);
        try request.respond(file_contents, .{
            .extra_headers = &.{
                .{ .name = "Cache-Control", .value = "no-cache" },
                .{ .name = "Connection", .value = "close" },
                .{ .name = "Content-Type", .value = content_type },
            },
        });
    } else {
        std.log.warn("got 404: {s}", .{target});
        try request.respond(html_404, .{
            .status = .not_found,
            .extra_headers = &.{
                .{ .name = "Cache-Control", .value = "no-cache" },
                .{ .name = "Connection", .value = "close" },
                .{ .name = "Content-Type", .value = "text/html" },
            },
        });
    }
}

fn determineContentType(file: []const u8) []const u8 {
    return std.StaticStringMap([]const u8).initComptime(.{
        .{ ".css", "text/css" },
        .{ ".html", "text/html" },
        .{ ".ico", "image/x-icon" },
        .{ ".js", "application/javascript" },
        .{ ".wasm", "application/wasm" },
    }).get(std.fs.path.extension(file)) orelse {
        std.log.err("failed to recognize file type for {s}", .{file});
        return "text/plain";
    };
}
