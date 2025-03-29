const std = @import("std");

const Ast = @import("re-fsm").Ast;
const Flavor = @import("re-fsm").Flavor;
const Nfa = @import("re-fsm").Nfa;
const minimumDfa = @import("re-fsm").minimumDfa;

const WasmInfo = struct {
    version: std.SemanticVersion,
    flavors: []const Flavor.Info,
    optimize: std.builtin.OptimizeMode,
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

export const info = std.fmt.comptimePrint("{}", .{std.json.fmt(WasmInfo{
    .version = @import("build-info").version,
    .flavors = &Flavor.infos,
    .optimize = @import("builtin").mode,
}, .{})}).*;

var gpa_instance: std.heap.GeneralPurposeAllocator(.{}) = .init;
export var flavor: Flavor = .default;
var regex_input: []u8 = &.{};
var input_string: []u8 = &.{};
var graph: [:0]const u8 = &.{};

const gpa = gpa_instance.allocator();

export fn allocate_regex_input(len: usize) ?[*]const u8 {
    std.log.info("allocating input regex", .{});
    regex_input = gpa.realloc(regex_input, len) catch {
        std.log.err("allocation for input regex failed", .{});
        return null;
    };
    return if (regex_input.len > 0) regex_input.ptr else null;
}

export fn allocate_input_string(len: usize) ?[*]const u8 {
    std.log.info("allocating input string", .{});
    input_string = gpa.realloc(input_string, len) catch {
        std.log.err("allocation for input string failed", .{});
        return null;
    };
    return if (input_string.len > 0) input_string.ptr else null;
}

export fn build_digraph() usize {
    std.log.info("building digraph regex:\"{s}\" flavor:\"{}\"", .{ regex_input, flavor });
    var ast = Ast.parse(gpa, flavor, regex_input) catch |err| switch (err) {
        error.ParseFail => return 0,
        else => {
            std.log.err("error while building ast: {}", .{err});
            return 0;
        },
    };
    defer ast.deinit(gpa);

    var nfa = Nfa.init(gpa, ast) catch |err| {
        std.log.err("error while building nfa: {}", .{err});
        return 0;
    };
    defer nfa.deinit(gpa);

    var dfa = minimumDfa(gpa, nfa) catch |err| {
        std.log.err("error while building dfa: {}", .{err});
        return 0;
    };
    defer dfa.deinit(gpa);

    if (graph.len > 0) {
        gpa.free(graph);
    }
    var al: std.ArrayList(u8) = .init(gpa);
    defer al.deinit();
    dfa.viz(al.writer()) catch |err| {
        std.log.err("error while building dot graph: {}", .{err});
        return 0;
    };
    graph = al.toOwnedSliceSentinel(0) catch |err| {
        std.log.err("error while building dot graph: {}", .{err});
        return 0;
    };

    return 1;
}

export fn run_digraph() usize {
    @panic("");
}

export fn render_digraph() ?[*:0]const u8 {
    return if (graph.len > 0) graph.ptr else null;
}
