const std = @import("std");

var input_regex: []u8 = &.{};
var output_digraph = std.ArrayList(u8).init(std.heap.wasm_allocator);

export fn alloc_input_regex(len: usize) ?[*]u8 {
    input_regex = std.heap.wasm_allocator.realloc(input_regex, len) catch {
        return null;
    };
    return input_regex.ptr;
}

export fn generate_digraph() void {
    output_digraph.clearRetainingCapacity();
    std.fmt.format(output_digraph.writer(),
        \\digraph {{
        \\  node [shape=circle];
        \\  0
        \\  node [shape=doublecircle];
        \\  1
        \\  0 -> 1 [label="{s}"];
        \\}}
    , .{input_regex}) catch return;
}

export fn get_digraph_addr() [*]const u8 {
    return output_digraph.items.ptr;
}

export fn get_digraph_length() usize {
    return output_digraph.items.len;
}
