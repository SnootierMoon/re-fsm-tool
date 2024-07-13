const std = @import("std");

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


var input_regex: []u8 = &.{};
var output_digraph = std.ArrayList(u8).init(std.heap.wasm_allocator);

export fn alloc_input_regex(len: usize) ?[*]u8 {
    input_regex = std.heap.wasm_allocator.realloc(input_regex, len) catch {
        std.log.err("failed to allocate {} bytes for input\n", .{len});
        return null;
    };
    return input_regex.ptr;
}

export fn generate_digraph() u32 {
    output_digraph.clearRetainingCapacity();
    @import("re_fsm.zig").run(output_digraph.writer(), std.heap.wasm_allocator, input_regex) catch |e| {
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
