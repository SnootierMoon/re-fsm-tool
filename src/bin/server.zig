const std = @import("std");

pub const options: std.Options = .{
    .log_level = switch (std.builtin.mode) {
        .Debug => .debug,
        else => .info,
    },
};

const web_files = @import("web-build").map;

var mem: [8096]u8 = undefined;

pub fn main() !void {
    var fba = std.heap.FixedBufferAllocator.init(&mem);

    var args = try std.process.argsWithAllocator(fba.allocator());
    const port = args: {
        std.debug.assert(args.skip());
        const port = if (args.next()) |port_arg|
            std.fmt.parseInt(u16, port_arg, 10) catch |err| switch (err) {
                else => break :args error.InvalidPort,
            }
        else
            16000;
        if (args.next()) |_| {
            break :args error.TooManyArguments;
        }
        break :args port;
    } catch |err| {
        try std.io.getStdErr().writer().print(
            \\Usage:
            \\    re-fsm-server [port]
            \\    by default, port is 16000
            \\
        , .{});
        return err;
    };

    const address = std.net.Address.parseIp("127.0.0.1", port) catch unreachable;
    var server = try address.listen(.{ .reuse_address = true });
    defer server.deinit();

    std.log.info("listening on http://127.0.0.1:{}", .{port});

    while (true) {
        const connection = try server.accept();
        defer connection.stream.close();
        handleConnection(connection) catch |err| {
            std.log.err("{s}", .{@errorName(err)});
        };
    }
}

fn handleConnection(connection: std.net.Server.Connection) !void {
    var http = std.http.Server.init(connection, &mem);
    var request = http.receiveHead() catch |err| switch (err) {
        error.HttpConnectionClosing => return,
        else => return err,
    };

    std.log.info("serving {s}", .{request.head.target});

    const req_file = if (request.head.target.len > 0 and request.head.target[0] == '/')
        request.head.target[1..]
    else
        request.head.target;

    if (req_file.len == 0) {
        try request.respond(comptime (web_files.get("index.html") orelse unreachable), .{
            .extra_headers = &.{
                .{ .name = "content-type", .value = "text/html" },
            },
        });
    } else if (web_files.get(req_file)) |file_contents|
        try request.respond(file_contents, .{
            .extra_headers = &.{
                .{ .name = "content-type", .value = determineContentType(req_file) },
            },
        })
    else {
        std.log.warn("404", .{});
        try request.respond(@"404", .{ .status = .not_found });
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

const @"404" =
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
