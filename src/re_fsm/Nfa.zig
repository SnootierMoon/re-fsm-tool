const std = @import("std");

const Ast = @import("Ast.zig");
const Nfa = @This();

reject_count: usize,
accept_count: usize,
edges: []Edge,

pub const Edge = struct {
    from: usize,
    sym: ?u7,
    to: usize,
};

pub fn fromAst(gpa: std.mem.Allocator, ast: Ast) !Nfa {
    var edges = std.ArrayList(Edge).init(gpa);
    defer edges.deinit();

    const reject_count = try gpa.alloc(usize, ast.nodes.len);
    defer gpa.free(reject_count);
    const accept_count = try gpa.alloc(usize, ast.nodes.len);
    defer gpa.free(accept_count);
    const reject_index = try gpa.alloc(usize, ast.nodes.len);
    defer gpa.free(reject_index);
    const accept_index = try gpa.alloc(usize, ast.nodes.len);
    defer gpa.free(accept_index);

    const ast_slice = ast.nodes.slice();
    const ast_tags = ast_slice.items(.tags);
    const ast_data = ast_slice.items(.data);

    for (ast_tags, 0..ast.nodes.len) |tag, index| {
        switch (tag) {
            .epsilon, .mask => {
                reject_count[index] = 1;
                accept_count[index] = 1;
            },
            .either => {
                const lhs_index = ast_data[index].either;
                reject_count[index] = 1 + reject_count[lhs_index] + reject_count[index - 1];
                accept_count[index] = accept_count[lhs_index] + accept_count[index - 1];
            },
            .concat => {
                const lhs_index = ast_data[index].concat;
                reject_count[index] = reject_count[lhs_index] + accept_count[lhs_index] + reject_count[index - 1];
                accept_count[index] = accept_count[index - 1];
            },
            .look_around => return error.Unimplemented,
            .repeat => {
                reject_count[index] = reject_count[index - 1];
                accept_count[index] = accept_count[index - 1];
            },
        }
    }

    reject_index[ast.nodes.len - 1] = 0;
    accept_index[ast.nodes.len - 1] = reject_count[ast.nodes.len - 1];
    for (0..ast.nodes.len) |index_rev| {
        const index = ast.nodes.len - index_rev - 1;
        switch (ast_tags[index]) {
            .epsilon, .mask => {},
            .either => {
                const lhs_index = ast_data[index].either;
                reject_index[lhs_index] = reject_index[index] + 1;
                accept_index[lhs_index] = accept_index[index];
                reject_index[index - 1] = reject_index[index] + 1 + reject_count[lhs_index];
                accept_index[index - 1] = accept_index[index] + accept_count[lhs_index];
            },
            .concat => {
                const lhs_index = ast_data[index].concat;
                reject_index[lhs_index] = reject_index[index];
                accept_index[lhs_index] = reject_index[index] + reject_count[lhs_index];
                reject_index[index - 1] = reject_index[index] + reject_count[lhs_index] + accept_count[lhs_index];
                accept_index[index - 1] = accept_index[index];
            },
            .look_around => return error.Unimplemented,
            .repeat => {
                reject_index[index - 1] = reject_index[index];
                accept_index[index - 1] = accept_index[index];
            },
        }
    }

    for (ast_tags, 0..ast.nodes.len) |tag, index| {
        switch (tag) {
            .epsilon => {
                try edges.append(.{ .from = reject_index[index], .sym = null, .to = accept_index[index] });
            },
            .mask => {
                for (0..128) |sym| {
                    if (ast_data[index].mask & (@as(u128, 1) << @intCast(sym)) != 0) {
                        try edges.append(.{ .from = reject_index[index], .sym = @intCast(sym), .to = accept_index[index] });
                    }
                }
            },
            .either => {
                const lhs_index = ast_data[index].either;
                try edges.append(.{ .from = reject_index[index], .sym = null, .to = reject_index[lhs_index] });
                try edges.append(.{ .from = reject_index[index], .sym = null, .to = reject_index[index - 1] });
            },
            .concat => {
                const lhs_index = ast_data[index].concat;
                for (0..accept_count[lhs_index]) |i| {
                    try edges.append(.{ .from = accept_index[lhs_index] + i, .sym = null, .to = reject_index[index - 1] });
                }
            },
            .look_around => return error.Unimplemented,
            .repeat => {
                for (0..accept_count[index - 1]) |i| {
                    try edges.append(.{ .from = accept_index[index - 1] + i, .sym = null, .to = reject_index[index - 1] });
                }
            },
        }
    }

    return .{
        .reject_count = reject_count[ast.nodes.len - 1],
        .accept_count = accept_count[ast.nodes.len - 1],
        .edges = try edges.toOwnedSlice(),
    };
}

pub fn deinit(nfa: *Nfa, gpa: std.mem.Allocator) void {
    gpa.free(nfa.edges);
}

pub fn viz(nfa: Nfa, writer: anytype) !void {
    try writer.print("digraph {{", .{});
    try writer.print(" node [shape=circle]", .{});
    for (0..nfa.reject_count) |i| {
        try writer.print(" {}\n", .{i});
    }
    try writer.print(" node [shape=doublecircle]", .{});
    for (0..nfa.accept_count) |i| {
        try writer.print(" {}\n", .{nfa.reject_count + i});
    }
    for (nfa.edges) |edge| {
        if (edge.sym) |sym| {
            if (sym == '\\' or sym == '\"' or !std.ascii.isAscii(sym)) {
                try writer.print(" {} -> {} [label=\"{}\"]", .{ edge.from, edge.to, sym });
            } else {
                try writer.print(" {} -> {} [label=\"{c}\"]", .{ edge.from, edge.to, sym });
            }
        } else {
            try writer.print(" {} -> {}", .{ edge.from, edge.to });
        }
    }
    try writer.print(" }}", .{});
}
