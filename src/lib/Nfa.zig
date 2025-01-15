const std = @import("std");

const Ast = @import("Ast.zig");
const Nfa = @This();

groups: []DigraphGroup,
arena: std.heap.ArenaAllocator.State,

pub const DigraphGroup = struct {
    total_states: usize,
    digraphs: []Digraph,
    edges: []Edge,
    gate_edges: []GateEdge,

    pub const empty: DigraphGroup = .{
        .total_states = 0,
        .digraphs = &.{},
        .edges = &.{},
        .gate_edges = &.{},
    };

    pub fn addEdge(group: *DigraphGroup, edge: Edge) void {
        const index = group.edges.len;
        group.edges.len += 1;
        group.edges[index] = edge;
    }

    pub fn addGateEdge(group: *DigraphGroup, gate_edge: GateEdge) void {
        const index = group.gate_edges.len;
        group.gate_edges.len += 1;
        group.gate_edges[index] = gate_edge;
    }

    pub fn allocateStatesFor(group: *DigraphGroup, digraph: *Digraph) void {
        digraph.state_index = group.total_states;
        group.total_states += digraph.ir_count + digraph.nr_count + digraph.ia_count + digraph.na_count;
    }
};

pub const Digraph = struct {
    state_index: usize,
    ir_count: usize, // initial, rejecting states
    nr_count: usize, // non-initial, rejecting states
    ia_count: usize, // initial, accepting states
    na_count: usize, // non-initial, accepting states

    pub inline fn ir_begin(digraph: Digraph) usize {
        return digraph.state_index;
    }

    pub inline fn ir_end(digraph: Digraph) usize {
        return digraph.state_index + digraph.ir_count;
    }

    pub const nr_begin = ir_end;

    pub inline fn nr_end(digraph: Digraph) usize {
        return digraph.state_index + digraph.ir_count + digraph.nr_count;
    }

    pub const ia_begin = nr_end;

    pub inline fn ia_end(digraph: Digraph) usize {
        return digraph.state_index + digraph.ir_count + digraph.nr_count + digraph.ia_count;
    }

    pub const na_begin = ia_end;

    pub inline fn na_end(digraph: Digraph) usize {
        return digraph.state_index + digraph.ir_count + digraph.nr_count + digraph.ia_count + digraph.na_count;
    }

    pub const reject_begin = ir_begin;
    pub const reject_end = nr_end;
    pub const accept_begin = ia_begin;
    pub const accept_end = na_end;
};

pub const Edge = struct {
    from: usize,
    to: usize,
    symbol: ?u7 = null,
};

pub const GateEdge = struct {
    from: usize,
    to: usize,
    digraph: usize,
    sign: Ast.LookAroundKind.Sign,
    deferred: bool,
};

pub fn init(gpa: std.mem.Allocator, ast: Ast) error{OutOfMemory}!Nfa {
    var arena_instance: std.heap.ArenaAllocator = .init(gpa);
    errdefer arena_instance.deinit();
    const arena = arena_instance.allocator();

    var num_groups: usize = 1;

    const AstNodeInfo = struct {
        lchild: usize,
        group: usize,
        digraph: usize,
        reject_index: usize,
        reject_count: usize,
        accept_index: usize,
        accept_count: usize,
    };

    var ast_info_list: std.MultiArrayList(AstNodeInfo) = .{};
    defer ast_info_list.deinit(gpa);
    try ast_info_list.resize(gpa, ast.node_tags.len);
    const ast_infos = ast_info_list.slice();
    const lchild_of_ast = ast_infos.items(.lchild);
    const group_of_ast = ast_infos.items(.group);
    const digraph_of_ast = ast_infos.items(.digraph);
    const reject_count_of_ast = ast_infos.items(.reject_count);
    const reject_index_of_ast = ast_infos.items(.reject_index);
    const accept_count_of_ast = ast_infos.items(.accept_count);
    const accept_index_of_ast = ast_infos.items(.accept_index);

    var it: Ast.Iterator = .init(ast);
    defer it.deinit(gpa);

    while (try it.next(gpa)) |ast_node| {
        const ast_index = it.index.node_tag - 1;
        switch (ast_node) {
            .symbol, .mask => {
                reject_count_of_ast[ast_index] = 1;
                accept_count_of_ast[ast_index] = 1;
            },
            .epsilon => {
                reject_count_of_ast[ast_index] = 0;
                accept_count_of_ast[ast_index] = 1;
            },
            .either => |either| {
                reject_count_of_ast[ast_index] = 1 + reject_count_of_ast[either.lhs] + reject_count_of_ast[either.rhs];
                accept_count_of_ast[ast_index] = accept_count_of_ast[either.lhs] + accept_count_of_ast[either.rhs];
                lchild_of_ast[ast_index] = either.lhs;
            },
            .concat => |concat| {
                reject_count_of_ast[ast_index] = reject_count_of_ast[concat.lhs] + accept_count_of_ast[concat.lhs] + reject_count_of_ast[concat.rhs];
                accept_count_of_ast[ast_index] = accept_count_of_ast[concat.rhs];
                lchild_of_ast[ast_index] = concat.lhs;
            },
            .repeat => |repeat| {
                reject_count_of_ast[ast_index] = reject_count_of_ast[repeat.inner];
                accept_count_of_ast[ast_index] = accept_count_of_ast[repeat.inner];
            },
            .look_around => |look_around| {
                reject_count_of_ast[ast_index] = 1;
                accept_count_of_ast[ast_index] = 1;
                if (look_around.kind.dir == .ahead) {
                    num_groups += 1;
                }
            },
        }
    }

    var groups = try arena.alloc(DigraphGroup, num_groups);
    groups.len = 1;
    groups[0].digraphs.len = 1;
    groups[0].edges.len = 0;
    groups[0].gate_edges.len = 0;
    group_of_ast[ast.node_tags.len - 1] = 0;
    digraph_of_ast[ast.node_tags.len - 1] = 0;

    var idx: Ast.Index = .end(ast);
    for (0..ast.node_tags.len) |ast_index_rev| {
        const ast_index = ast.node_tags.len - ast_index_rev - 1;
        switch (ast.node_tags[ast_index]) {
            .symbol => {
                _ = idx.prevSymbol(ast);
                groups[group_of_ast[ast_index]].edges.len += 1;
            },
            .epsilon => {
                groups[group_of_ast[ast_index]].edges.len += 1;
            },
            .mask => {
                groups[group_of_ast[ast_index]].edges.len += @popCount(idx.prevMask(ast));
            },
            .either => {
                group_of_ast[lchild_of_ast[ast_index]] = group_of_ast[ast_index];
                digraph_of_ast[lchild_of_ast[ast_index]] = digraph_of_ast[ast_index];
                group_of_ast[ast_index - 1] = group_of_ast[ast_index];
                digraph_of_ast[ast_index - 1] = digraph_of_ast[ast_index];
                groups[group_of_ast[ast_index]].edges.len += 2;
            },
            .concat => {
                group_of_ast[lchild_of_ast[ast_index]] = group_of_ast[ast_index];
                digraph_of_ast[lchild_of_ast[ast_index]] = digraph_of_ast[ast_index];
                group_of_ast[ast_index - 1] = group_of_ast[ast_index];
                digraph_of_ast[ast_index - 1] = digraph_of_ast[ast_index];
                groups[group_of_ast[ast_index]].edges.len += accept_count_of_ast[lchild_of_ast[ast_index]];
            },
            .repeat => {
                group_of_ast[ast_index - 1] = group_of_ast[ast_index];
                digraph_of_ast[ast_index - 1] = digraph_of_ast[ast_index];
                groups[group_of_ast[ast_index]].edges.len += accept_count_of_ast[ast_index - 1];
            },
            .neg_look_behind, .pos_look_behind => {
                const group_index = group_of_ast[ast_index];
                const group = &groups[group_index];
                const child_digraph_index = group.digraphs.len;
                group.digraphs.len += 1;
                group_of_ast[ast_index - 1] = group_index;
                digraph_of_ast[ast_index - 1] = child_digraph_index;
                group.gate_edges.len += 1;
            },
            .neg_look_ahead, .pos_look_ahead => {
                const group_index = group_of_ast[ast_index];
                const group = &groups[group_index];
                const child_group_index = groups.len;
                groups.len += 1;
                const child_group = &groups[child_group_index];
                group_of_ast[ast_index - 1] = child_group_index;
                digraph_of_ast[ast_index - 1] = 0;
                child_group.digraphs.len = 1;
                child_group.edges.len = 0;
                child_group.gate_edges.len = 0;
                group.gate_edges.len += 1;
            },
        }
    }

    for (groups) |*group| {
        group.total_states = 0;
        group.digraphs = try arena.alloc(Digraph, group.digraphs.len);
        group.edges = try arena.alloc(Edge, group.edges.len);
        group.edges.len = 0;
        group.gate_edges = try arena.alloc(GateEdge, group.gate_edges.len);
        group.gate_edges.len = 0;
    }

    const root_group = &groups[0];
    const root_digraph = &root_group.digraphs[0];
    root_digraph.ir_count = 1;
    root_digraph.nr_count = reject_count_of_ast[ast.node_tags.len - 1] - 1;
    root_digraph.ia_count = 0;
    root_digraph.na_count = accept_count_of_ast[ast.node_tags.len - 1];
    root_group.allocateStatesFor(root_digraph);
    reject_index_of_ast[ast.node_tags.len - 1] = 0;
    accept_index_of_ast[ast.node_tags.len - 1] = reject_count_of_ast[ast.node_tags.len - 1];
    idx = .end(ast);
    for (0..ast.node_tags.len) |ast_index_rev| {
        const ast_index = ast.node_tags.len - ast_index_rev - 1;
        switch (ast.node_tags[ast_index]) {
            .symbol => {
                groups[group_of_ast[ast_index]].addEdge(.{
                    .from = reject_index_of_ast[ast_index],
                    .to = accept_index_of_ast[ast_index],
                    .symbol = idx.prevSymbol(ast),
                });
            },
            .mask => {
                const mask = idx.prevMask(ast);
                for (0..128) |symbol| {
                    if (mask & (@as(u128, 1) << @intCast(symbol)) != 0) {
                        groups[group_of_ast[ast_index]].addEdge(.{
                            .from = reject_index_of_ast[ast_index],
                            .to = accept_index_of_ast[ast_index],
                            .symbol = @intCast(symbol),
                        });
                    }
                }
            },
            .epsilon => {
                groups[group_of_ast[ast_index]].addEdge(.{
                    .from = reject_index_of_ast[ast_index],
                    .to = accept_index_of_ast[ast_index],
                    .symbol = null,
                });
            },
            .either => {
                reject_index_of_ast[lchild_of_ast[ast_index]] = reject_index_of_ast[ast_index] + 1;
                accept_index_of_ast[lchild_of_ast[ast_index]] = accept_index_of_ast[ast_index];
                reject_index_of_ast[ast_index - 1] = reject_index_of_ast[ast_index] + 1 + reject_count_of_ast[lchild_of_ast[ast_index]];
                accept_index_of_ast[ast_index - 1] = accept_index_of_ast[ast_index] + accept_count_of_ast[lchild_of_ast[ast_index]];
                groups[group_of_ast[ast_index]].addEdge(.{
                    .from = reject_index_of_ast[ast_index],
                    .to = reject_index_of_ast[lchild_of_ast[ast_index]],
                    .symbol = null,
                });
                groups[group_of_ast[ast_index]].addEdge(.{
                    .from = reject_index_of_ast[ast_index],
                    .to = reject_index_of_ast[ast_index - 1],
                    .symbol = null,
                });
            },
            .concat => {
                reject_index_of_ast[lchild_of_ast[ast_index]] = reject_index_of_ast[ast_index];
                accept_index_of_ast[lchild_of_ast[ast_index]] = reject_index_of_ast[ast_index] + reject_count_of_ast[lchild_of_ast[ast_index]];
                reject_index_of_ast[ast_index - 1] = reject_index_of_ast[ast_index] + reject_count_of_ast[lchild_of_ast[ast_index]] + accept_count_of_ast[lchild_of_ast[ast_index]];
                accept_index_of_ast[ast_index - 1] = accept_index_of_ast[ast_index];
                for (accept_index_of_ast[lchild_of_ast[ast_index]]..accept_index_of_ast[lchild_of_ast[ast_index]] + accept_count_of_ast[lchild_of_ast[ast_index]]) |from| {
                    groups[group_of_ast[ast_index]].addEdge(.{
                        .from = from,
                        .to = reject_index_of_ast[ast_index - 1],
                        .symbol = null,
                    });
                }
            },
            .repeat => {
                reject_index_of_ast[ast_index - 1] = reject_index_of_ast[ast_index];
                accept_index_of_ast[ast_index - 1] = accept_index_of_ast[ast_index];
                for (accept_index_of_ast[ast_index - 1]..accept_index_of_ast[ast_index - 1] + accept_count_of_ast[ast_index - 1]) |from| {
                    groups[group_of_ast[ast_index]].addEdge(.{
                        .from = from,
                        .to = reject_index_of_ast[ast_index - 1],
                        .symbol = null,
                    });
                }
            },
            .neg_look_behind, .pos_look_behind, .neg_look_ahead, .pos_look_ahead => {
                const look_around = ast.node_tags[ast_index].toLookAround();
                const child_group = &groups[group_of_ast[ast_index - 1]];
                const child_digraph = &child_group.digraphs[digraph_of_ast[ast_index - 1]];
                switch (look_around.dir) {
                    .behind => {
                        groups[group_of_ast[ast_index]].addGateEdge(.{
                            .from = reject_index_of_ast[ast_index],
                            .to = accept_index_of_ast[ast_index],
                            .digraph = digraph_of_ast[ast_index - 1],
                            .sign = look_around.sign,
                            .deferred = false,
                        });
                    },
                    .ahead => {
                        groups[group_of_ast[ast_index]].addGateEdge(.{
                            .from = reject_index_of_ast[ast_index],
                            .to = accept_index_of_ast[ast_index],
                            .digraph = group_of_ast[ast_index - 1],
                            .sign = look_around.sign,
                            .deferred = true,
                        });
                    },
                }
                child_digraph.ir_count = 1;
                child_digraph.nr_count = reject_count_of_ast[ast_index - 1] - 1;
                child_digraph.ia_count = 0;
                child_digraph.na_count = accept_count_of_ast[ast_index - 1];
                child_group.allocateStatesFor(child_digraph);
                reject_index_of_ast[ast_index - 1] = child_digraph.reject_begin();
                accept_index_of_ast[ast_index - 1] = child_digraph.accept_begin();
            },
        }
    }

    return .{ .arena = arena_instance.state, .groups = groups };
}

pub fn deinit(nfa: *Nfa, gpa: std.mem.Allocator) void {
    const arena = nfa.arena.promote(gpa);
    arena.deinit();
}
