const std = @import("std");

const Ast = @import("re-fsm").Ast;
const Flavor = @import("re-fsm").Flavor;

const VersionInfo = struct {
    version: std.SemanticVersion = std.SemanticVersion.parse("0.0.1") catch unreachable,
    flavors: []const Flavor = std.enums.values(Flavor),
    optimize: std.builtin.OptimizeMode = @import("builtin").mode,
};

const js = struct {
    extern "js" fn panic() noreturn;
    extern "js" fn console_log_write(ptr: [*]const u8, len: usize) void;
    extern "js" fn console_log_flush(level: u8) void;
};

pub fn panic(msg: []const u8, stack_trace: ?*std.builtin.StackTrace, ret_addr: ?usize) noreturn {
    _ = ret_addr;
    _ = stack_trace;
    std.log.err("panic: {s}", .{msg});
    js.panic();
}

fn log(
    comptime message_level: std.log.Level,
    comptime scope: @TypeOf(.enum_literal),
    comptime format: []const u8,
    args: anytype,
) void {
    _ = scope;
    var buffered = std.io.bufferedWriter(ConsoleLogWriter{ .context = {} });
    std.fmt.format(buffered.writer(), format, args) catch comptime unreachable;
    buffered.flush() catch comptime unreachable;
    js.console_log_flush(switch (message_level) {
        .err => 4,
        .warn => 3,
        .info => 2,
        .debug => 1,
    });
}
const ConsoleLogWriter = std.io.GenericWriter(void, error{}, struct {
    fn writeFn(context: void, bytes: []const u8) error{}!usize {
        _ = context;
        js.console_log_write(bytes.ptr, bytes.len);
        return bytes.len;
    }
}.writeFn);

pub const std_options: std.Options = .{
    .logFn = log,
};

export const version_info = std.fmt.comptimePrint("{}", .{std.json.fmt(VersionInfo{}, .{})}).*;

var gpa_instance: std.heap.GeneralPurposeAllocator(.{}) = .init;
var input_regex: []u8 = &.{};
var input_string: []u8 = &.{};

const allocator = gpa_instance.allocator();

export fn allocate_input_regex(len: usize) [*]const u8 {
    allocator.free(input_regex);
    input_regex = allocator.alloc(u8, len) catch &.{};
    return input_regex.ptr;
}

export fn allocate_input_string(len: usize) [*]const u8 {
    allocator.free(input_string);
    input_string = allocator.alloc(u8, len) catch &.{};
    return input_string.ptr;
}

export fn build_digraph() void {
    var ast: Ast = Ast.parse(allocator, .posix_ere, input_regex) catch return;
    defer ast.deinit(allocator);

    std.log.info("{}", .{ast});
}

export fn run_digraph() void {
    @panic("");
}

export fn render_digraph() [*:0]const u8 {
    @panic("");
}
