const std = @import("std");

const Ast = @import("re-fsm").Ast;
const minimumDfa = @import("re-fsm").minimumDfa;
const Flavor = @import("re-fsm").Flavor;
const Nfa = @import("re-fsm").Nfa;

pub const std_options: std.Options = .{
    .fmt_max_depth = 5,
};

const flavor_names = std.EnumArray(Flavor, []const u8).init(.{
    .pcre = "PCRE",
    .posix_bre = "POSIX BRE",
    .posix_ere = "POSIX ERE",
});

pub fn main() !void {
    var gpa_instance: std.heap.GeneralPurposeAllocator(.{}) = .init;
    defer _ = gpa_instance.deinit();
    const gpa = gpa_instance.allocator();

    const stdin = std.io.getStdIn();
    const stderr = std.io.getStdErr();

    if (!std.posix.isatty(stdin.handle)) {
        try stderr.writer().writeAll("Needs a TTY!");
        return error.NotATerminal;
    }

    var args = try std.process.argsWithAllocator(gpa);
    const flavor = args: {
        std.debug.assert(args.skip());
        const flavor = if (args.next()) |flavor_arg|
            std.meta.stringToEnum(Flavor, flavor_arg) orelse break :args error.InvalidFlavor
        else
            .posix_ere;
        if (args.next()) |_| {
            break :args error.TooManyArguments;
        }
        break :args flavor;
    } catch |err| {
        try stderr.writer().writeAll(
            \\Usage:
            \\    re-fsm-cli [flavor]
            \\    flavor is one of pcre, posix_bre, or posix_ere
            \\    by default, flavor is posix_ere
            \\
        );
        return err;
    };

    var in_buffered = std.io.bufferedReader(stdin.reader());
    var buffered_out = std.io.bufferedWriter(stderr.writer());

    var input_buf: std.ArrayList(u8) = .init(gpa);
    defer input_buf.deinit();
    while (true) {
        try buffered_out.writer().print("Enter Regex ({s}): ", .{flavor_names.get(flavor)});
        try buffered_out.flush();
        input_buf.clearRetainingCapacity();
        in_buffered.reader().streamUntilDelimiter(input_buf.writer(), '\n', null) catch |err| switch (err) {
            error.EndOfStream => break,
            else => return err,
        };

        var ast: Ast = Ast.parse(gpa, flavor, input_buf.items) catch |err| switch (err) {
            error.ParseFail => {
                try buffered_out.writer().print("parse failed\n", .{});
                continue;
            },
            else => return err,
        };
        defer ast.deinit(gpa);

        var nfa: Nfa = try .init(gpa, ast);
        defer nfa.deinit(gpa);

        var dfa = try minimumDfa(gpa, nfa);
        defer dfa.deinit(gpa);

        try dfa.viz(buffered_out.writer());
        try buffered_out.writer().writeByte('\n');
    }
    try buffered_out.writer().print("\x1b[2K\r", .{});
    try buffered_out.flush();
}