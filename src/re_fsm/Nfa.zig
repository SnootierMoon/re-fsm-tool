const std = @import("std");

const Ast = @import("Ast.zig");
const Nfa = @This();

groups: std.ArrayListUnmanaged(DigraphGroup) = .{},

pub const DigraphGroup = struct {
    total_states: usize,
    digraphs: std.ArrayListUnmanaged(Digraph) = .{},
    edges: std.ArrayListUnmanaged(Edge) = .{},
    gate_edges: std.ArrayListUnmanaged(GateEdge) = .{},
    deferred_gate_edges: std.ArrayListUnmanaged(GateEdge) = .{},

    pub fn deinit(group: *DigraphGroup, gpa: std.mem.Allocator) void {
        group.digraphs.deinit(gpa);
        group.edges.deinit(gpa);
        group.gate_edges.deinit(gpa);
        group.deferred_gate_edges.deinit(gpa);
    }
};

pub const Digraph = struct {
    state_index: usize,
    reject_count: usize,
    accept_count: usize,
};

pub const Edge = struct {
    from: usize,
    to: usize,
    sym: ?u7 = null,
};

pub const GateEdge = struct {
    from: usize,
    to: usize,
    digraph: usize,
    sign: Ast.LookAround.Sign,
};

pub fn init(gpa: std.mem.Allocator, ast: Ast) error{OutOfMemory}!Nfa {
    var nfa: Nfa = .{};
    errdefer nfa.deinit(gpa);

    const NodeNfaInfo = struct {
        digraph_group: usize,
        digraph: usize,
        reject_index: usize,
        accept_index: usize,
        reject_count: usize,
        accept_count: usize,
    };
    var node_nfa_infos: std.MultiArrayList(NodeNfaInfo) = .{};
    defer node_nfa_infos.deinit(gpa);
    try node_nfa_infos.resize(gpa, ast.nodes.len);
    const node_nfa_infos_slice = node_nfa_infos.slice();
    const node_digraph_group = node_nfa_infos_slice.items(.digraph_group);
    const node_digraph = node_nfa_infos_slice.items(.digraph);
    const node_reject_index = node_nfa_infos_slice.items(.reject_index);
    const node_accept_index = node_nfa_infos_slice.items(.accept_index);
    const node_reject_count = node_nfa_infos_slice.items(.reject_count);
    const node_accept_count = node_nfa_infos_slice.items(.accept_count);

    const ast_slice = ast.nodes.slice();
    const ast_tags = ast_slice.items(.tags);
    const ast_data = ast_slice.items(.data);

    for (ast_tags, 0..) |tag, node| {
        switch (tag) {
            .epsilon, .mask => {
                node_reject_count[node] = 1;
                node_accept_count[node] = 1;
            },
            .either => {
                const lhs_node = ast_data[node].either;
                const rhs_node = node - 1;
                node_reject_count[node] = 1 + node_reject_count[lhs_node] + node_reject_count[rhs_node];
                node_accept_count[node] = node_accept_count[lhs_node] + node_accept_count[rhs_node];
            },
            .concat => {
                const lhs_node = ast_data[node].concat;
                const rhs_node = node - 1;
                node_reject_count[node] = node_reject_count[lhs_node] + node_accept_count[lhs_node] + node_reject_count[rhs_node];
                node_accept_count[node] = node_accept_count[rhs_node];
            },
            .look_around => {
                node_reject_count[node] = 1;
                node_accept_count[node] = 1;
            },
            .repeat => {
                const inner_node = node - 1;
                node_reject_count[node] = node_reject_count[inner_node];
                node_accept_count[node] = node_accept_count[inner_node];
            },
        }
    }

    try nfa.groups.append(gpa, .{ .total_states = node_reject_count[ast.nodes.len - 1] + node_accept_count[ast.nodes.len - 1] });
    try nfa.groups.items[0].digraphs.append(gpa, .{
        .state_index = 0,
        .reject_count = node_reject_count[ast.nodes.len - 1],
        .accept_count = node_accept_count[ast.nodes.len - 1],
    });
    node_digraph_group[ast.nodes.len - 1] = 0;
    node_digraph[ast.nodes.len - 1] = 0;
    node_reject_index[ast.nodes.len - 1] = 0;
    node_accept_index[ast.nodes.len - 1] = node_reject_count[ast.nodes.len - 1];

    for (0..ast.nodes.len) |node_rev| {
        const node = ast.nodes.len - node_rev - 1;
        switch (ast_tags[node]) {
            .epsilon, .mask => {},
            .either => {
                const lhs_node = ast_data[node].either;
                const rhs_node = node - 1;
                node_digraph_group[lhs_node] = node_digraph_group[node];
                node_digraph[lhs_node] = node_digraph[node];
                node_reject_index[lhs_node] = node_reject_index[node] + 1;
                node_accept_index[lhs_node] = node_accept_index[node];
                node_digraph_group[rhs_node] = node_digraph_group[node];
                node_digraph[rhs_node] = node_digraph[node];
                node_reject_index[rhs_node] = node_reject_index[node] + 1 + node_reject_count[lhs_node];
                node_accept_index[rhs_node] = node_accept_index[node] + node_accept_count[lhs_node];
            },
            .concat => {
                const lhs_node = ast_data[node].concat;
                const rhs_node = node - 1;
                node_digraph_group[lhs_node] = node_digraph_group[node];
                node_digraph[lhs_node] = node_digraph[node];
                node_reject_index[lhs_node] = node_reject_index[node];
                node_accept_index[lhs_node] = node_reject_index[node] + node_reject_count[lhs_node];
                node_digraph_group[rhs_node] = node_digraph_group[node];
                node_digraph[rhs_node] = node_digraph[node];
                node_reject_index[rhs_node] = node_reject_index[node] + node_reject_count[lhs_node] + node_accept_count[lhs_node];
                node_accept_index[rhs_node] = node_accept_index[node];
            },
            .look_around => {
                const inner_node = node - 1;
                switch (ast_data[node].look_around.dir) {
                    .ahead => {
                        node_digraph_group[inner_node] = nfa.groups.items.len;
                        try nfa.groups.append(gpa, .{ .total_states = undefined });
                    },
                    .behind => {
                        node_digraph_group[inner_node] = node_digraph_group[node];
                    },
                }

                const inner_group = &nfa.groups.items[node_digraph_group[inner_node]];
                node_digraph[inner_node] = inner_group.digraphs.items.len;
                const loop_state = @intFromBool(ast_data[node].look_around.dir == .behind);
                try inner_group.digraphs.append(gpa, .{
                    .state_index = inner_group.total_states,
                    .reject_count = loop_state + node_reject_count[inner_node],
                    .accept_count = node_accept_count[inner_node],
                });
                node_reject_index[inner_node] = loop_state + inner_group.total_states;
                node_accept_index[inner_node] = loop_state + inner_group.total_states + node_reject_count[inner_node];
                inner_group.total_states += loop_state + node_reject_count[inner_node] + node_accept_count[inner_node];
            },
            .repeat => {
                const inner_node = node - 1;
                node_reject_index[inner_node] = node_reject_index[node];
                node_accept_index[inner_node] = node_accept_index[node];
                node_digraph_group[inner_node] = node_digraph_group[node];
                node_digraph[inner_node] = node_digraph[node];
            },
        }
    }

    for (ast_tags, 0..) |tag, node| {
        switch (tag) {
            .epsilon => {
                const group = &nfa.groups.items[node_digraph_group[node]];
                try group.edges.append(gpa, .{ .from = node_reject_index[node], .to = node_accept_index[node] });
            },
            .mask => {
                const group = &nfa.groups.items[node_digraph_group[node]];
                for (0..128) |sym| {
                    if (ast_data[node].mask & (@as(u128, 1) << @intCast(sym)) != 0) {
                        try group.edges.append(gpa, .{ .from = node_reject_index[node], .to = node_accept_index[node], .sym = @intCast(sym) });
                    }
                }
            },
            .either => {
                const group = &nfa.groups.items[node_digraph_group[node]];
                const lhs_node = ast_data[node].either;
                const rhs_node = node - 1;
                try group.edges.append(gpa, .{ .from = node_reject_index[node], .to = node_reject_index[lhs_node] });
                try group.edges.append(gpa, .{ .from = node_reject_index[node], .to = node_reject_index[rhs_node] });
            },
            .concat => {
                const group = &nfa.groups.items[node_digraph_group[node]];
                const lhs_node = ast_data[node].concat;
                const rhs_node = node - 1;
                for (0..node_accept_count[lhs_node]) |lhs_node_accept| {
                    try group.edges.append(gpa, .{ .from = node_accept_index[lhs_node] + lhs_node_accept, .to = node_reject_index[rhs_node] });
                }
            },
            .look_around => {
                const group = &nfa.groups.items[node_digraph_group[node]];
                const inner_node = node - 1;
                switch (ast_data[node].look_around.dir) {
                    .ahead => {
                        try group.deferred_gate_edges.append(gpa, .{
                            .from = node_reject_index[node],
                            .to = node_accept_index[node],
                            .digraph = node_digraph_group[inner_node],
                            .sign = ast_data[node].look_around.sign,
                        });
                    },
                    .behind => {
                        const state_index = group.digraphs.items[node_digraph[inner_node]].state_index;
                        try group.edges.append(gpa, .{ .from = 0, .to = state_index });
                        for (0..128) |sym| {
                            try group.edges.append(gpa, .{ .from = state_index, .to = state_index, .sym = @intCast(sym) });
                        }
                        try group.edges.append(gpa, .{ .from = state_index, .to = state_index + 1 });
                        try group.gate_edges.append(gpa, .{
                            .from = node_reject_index[node],
                            .to = node_accept_index[node],
                            .digraph = node_digraph[inner_node],
                            .sign = ast_data[node].look_around.sign,
                        });
                    },
                }
            },
            .repeat => {
                const group = &nfa.groups.items[node_digraph_group[node]];
                const inner_node = node - 1;
                for (0..node_accept_count[inner_node]) |inner_node_accept| {
                    try group.edges.append(gpa, .{ .from = node_accept_index[inner_node] + inner_node_accept, .to = node_reject_index[inner_node] });
                }
            },
        }
    }

    return nfa;
}

pub fn deinit(nfa: *Nfa, gpa: std.mem.Allocator) void {
    for (nfa.groups.items) |*group| {
        group.deinit(gpa);
    }
    nfa.groups.deinit(gpa);
}

pub fn dbgPrint(nfa: Nfa, writer: anytype) !void {
    for (nfa.groups.items, 0..) |group, group_index| {
        try writer.print("Digraph group {}\n", .{group_index});
        for (group.digraphs.items, 0..) |digraph, digraph_index| {
            try writer.print(" - Digraph {}\n", .{digraph_index});
            try writer.print("   {} reject, {} accept, @ {}\n", .{ digraph.reject_count, digraph.accept_count, digraph.state_index });
        }
        try writer.print(" - Edges\n", .{});
        for (group.edges.items) |edge| {
            if (edge.sym) |c| {
                try writer.print("   {} -> {} ({c})\n", .{ edge.from, edge.to, @as(u8, c) });
            } else {
                try writer.print("   {} -> {} (null)\n", .{ edge.from, edge.to });
            }
        }
        try writer.print(" - Gate Edges\n", .{});
        for (group.gate_edges.items) |gate_edge| {
            try writer.print("   {} -> {} (digraph={}, sign={})\n", .{
                gate_edge.from,
                gate_edge.to,
                gate_edge.digraph,
                gate_edge.sign == .pos,
            });
        }

        try writer.print(" - Deferred Gate Edges\n", .{});
        for (group.deferred_gate_edges.items) |gate_edge| {
            try writer.print("   {} -> {} (digraph={}, sign={})\n", .{
                gate_edge.from,
                gate_edge.to,
                gate_edge.digraph,
                gate_edge.sign == .pos,
            });
        }
    }
}
