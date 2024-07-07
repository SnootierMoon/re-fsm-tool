const std = @import("std");
const ReAst = @import("re_fsm.zig").ReAst;
const Nfa = @import("re_fsm.zig").Nfa;
const Dfa = @import("re_fsm.zig").Dfa;

pub const std_options: std.Options = .{
    .logFn = logImpl,
};

extern fn console_print(lvl: u8, ptr: [*]const u8, len: usize) void;

fn logImpl(
    comptime message_level: std.log.Level,
    comptime scope: @TypeOf(.enum_literal),
    comptime format: []const u8,
    args: anytype,
) void {
    _ = scope;
    var buf: [1 << 16]u8 = undefined;
    const lvl: u8 = switch (message_level) {
        .err => 3,
        .warn => 2,
        .info => 1,
        .debug => 0,
    };
    const msg = std.fmt.bufPrint(&buf, format, args) catch
        "failed to print a log message!";
    console_print(lvl, msg.ptr, msg.len);
}

fn run() !void {
    std.log.debug("start ast parse", .{});
    var ast = try ReAst.parse(std.heap.wasm_allocator, input_regex);
    defer ast.deinit(std.heap.wasm_allocator);
    std.log.debug("end ast parse", .{});

    output_digraph.clearRetainingCapacity();
    // try ast.viz(output_digraph.writer());

    std.log.debug("start nfa gen", .{});
    var nfa = try Nfa.fromReAst(std.heap.wasm_allocator, ast);
    defer nfa.deinit(std.heap.wasm_allocator);
    std.log.debug("end ast gen", .{});

    std.log.debug("start nfa viz", .{});
    try nfa.viz(output_digraph.writer());
    std.log.info("end nfa viz", .{});

    var dfa = try Dfa.fromNfa(std.heap.wasm_allocator, nfa);
    defer dfa.deinit(std.heap.wasm_allocator);
}

var input_regex: []u8 = &.{};
var output_digraph = std.ArrayList(u8).init(std.heap.wasm_allocator);

export fn alloc_input_regex(len: usize) ?[*]u8 {
    input_regex = std.heap.wasm_allocator.realloc(input_regex, len) catch {
        std.log.err("failed to allocate {} bytes for input\n", .{len});
        return null;
    };
    std.log.debug("allocated {} bytes for input\n", .{len});
    return input_regex.ptr;
}

export fn generate_digraph() u32 {
    run() catch |e| {
        std.log.warn("{s}", .{@errorName(e)});
        return 0;
    };
    return 1;
}

export fn get_digraph_addr() [*]const u8 {
    return output_digraph.items.ptr;
}

export fn get_digraph_length() usize {
    return output_digraph.items.len;
}
