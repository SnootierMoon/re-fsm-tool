const std = @import("std");

const Dfa = @This();
const Nfa = @import("Nfa.zig");
const printMask = @import("root.zig").printMask;

arena: std.heap.ArenaAllocator.State,
states: std.MultiArrayList(State),

const State = struct {
    final: bool,
    edge_mask: u128,
    edges: []usize,

    fn init(final: bool) State {
        return .{ .final = final, .edge_mask = 0, .edges = &.{} };
    }
};

const set = struct {
    fn find(items: []const usize, key: usize) struct { bool, usize } {
        var lo: usize = 0;
        var hi = items.len;
        while (lo < hi) {
            const mid = lo + (hi - lo) / 2;
            if (items[mid] >= key) {
                hi = mid;
            } else {
                lo = mid + 1;
            }
        }
        return .{ lo < items.len and items[lo] == key, lo };
    }

    fn insert(arena: std.mem.Allocator, items: *std.ArrayListUnmanaged(usize), key: usize) error{OutOfMemory}!bool {
        const found, const index = find(items.items, key);
        if (!found) {
            try items.insert(arena, index, key);
            return true;
        }
        return false;
    }

    const HashCtx = struct {
        pub fn hash(ctx: HashCtx, k: []const usize) u32 {
            _ = ctx;
            return @truncate(std.hash.Wyhash.hash(0, std.mem.sliceAsBytes(k)));
        }
        pub fn eql(ctx: HashCtx, k1: []const usize, k2: []const usize, k2_index: usize) bool {
            _ = .{ ctx, k2_index };
            return std.mem.eql(usize, k1, k2);
        }
    };
};

const NfaEdges = struct {
    symbol_map: NfaSymbolEdgeMap,
    eps_map: NfaEpsilonEdgeMap,
    state_set: std.ArrayListUnmanaged(usize),
    pq: EpsClosurePriorityQueue,

    const EpsClosurePriorityQueue = std.PriorityQueue(usize, void, struct {
        fn f(ctx: void, lhs: usize, rhs: usize) std.math.Order {
            _ = ctx;
            return std.math.order(rhs, lhs);
        }
    }.f);

    const NfaSymbolEdgeMap = std.AutoHashMapUnmanaged(struct { usize, u7 }, std.ArrayListUnmanaged(usize));
    const NfaEpsilonEdgeMap = std.AutoHashMapUnmanaged(usize, struct { to: std.ArrayListUnmanaged(usize), gates: std.ArrayListUnmanaged(usize) });

    fn init(arena: std.mem.Allocator, group: Nfa.DigraphGroup) error{OutOfMemory}!NfaEdges {
        var symbol_map: NfaSymbolEdgeMap = .empty;
        var eps_map: NfaEpsilonEdgeMap = .empty;
        for (group.edges) |edge| {
            if (edge.symbol) |symbol| {
                const entry = try symbol_map.getOrPutValue(arena, .{ edge.from, symbol }, .empty);
                try entry.value_ptr.append(arena, edge.to);
            } else {
                const entry = try eps_map.getOrPutValue(arena, edge.from, .{ .to = .empty, .gates = .empty });
                try entry.value_ptr.to.append(arena, edge.to);
            }
        }
        for (group.gate_edges, 0..) |gate_edge, gate_edge_index| {
            if (!gate_edge.deferred) {
                const entry = try eps_map.getOrPutValue(arena, gate_edge.from, .{ .to = .empty, .gates = .empty });
                try entry.value_ptr.gates.append(arena, gate_edge_index);
            }
        }

        return .{
            .symbol_map = symbol_map,
            .eps_map = eps_map,
            .state_set = .empty,
            .pq = .init(arena, {}),
        };
    }

    fn eClosure(nfa_edges: *NfaEdges, arena: std.mem.Allocator, group: Nfa.DigraphGroup) error{OutOfMemory}!void {
        // https://github.com/ziglang/zig/pull/20282
        nfa_edges.pq.items.len = 0;
        try nfa_edges.pq.addSlice(nfa_edges.state_set.items);
        while (nfa_edges.pq.removeOrNull()) |state| {
            if (nfa_edges.eps_map.get(state)) |value| {
                for (value.to.items) |to_state| {
                    if (try set.insert(arena, &nfa_edges.state_set, to_state)) {
                        try nfa_edges.pq.add(to_state);
                    }
                }
                for (value.gates.items) |gate_edge_index| {
                    const gate_edge = group.gate_edges[gate_edge_index];
                    const digraph = group.digraphs[gate_edge.digraph];
                    _, const final_state = set.find(nfa_edges.state_set.items, digraph.accept_begin());
                    if ((final_state < nfa_edges.state_set.items.len and nfa_edges.state_set.items[final_state] < digraph.accept_end()) == (gate_edge.sign == .pos)) {
                        if (try set.insert(arena, &nfa_edges.state_set, gate_edge.to)) {
                            try nfa_edges.pq.add(gate_edge.to);
                        }
                    }
                }
            }
        }
        for (group.digraphs[1..]) |digraph| {
            for (digraph.state_index..digraph.state_index + digraph.ir_count) |state| {
                _ = try set.insert(arena, &nfa_edges.state_set, state);
            }
            for (digraph.state_index + digraph.ir_count + digraph.nr_count..digraph.state_index + digraph.ir_count + digraph.nr_count + digraph.ia_count) |state| {
                _ = try set.insert(arena, &nfa_edges.state_set, state);
            }
        }
    }

    fn move(nfa_edges: *NfaEdges, arena: std.mem.Allocator, from: []const usize, symbol: u7) error{OutOfMemory}!void {
        nfa_edges.state_set.clearRetainingCapacity();
        for (from) |from_state| {
            if (nfa_edges.symbol_map.get(.{ from_state, symbol })) |to_states| {
                for (to_states.items) |to_state| {
                    _ = try set.insert(arena, &nfa_edges.state_set, to_state);
                }
            }
        }
    }
};

pub fn init(gpa: std.mem.Allocator, group: Nfa.DigraphGroup) error{OutOfMemory}!struct { Dfa, []Nfa.GateEdge } {
    var work_arena_instance: std.heap.ArenaAllocator = .init(gpa);
    defer work_arena_instance.deinit();
    const work_arena = work_arena_instance.allocator();

    var deferred_gates: std.ArrayListUnmanaged(Nfa.GateEdge) = .empty;
    defer deferred_gates.deinit(gpa);

    var nfa_edges = try NfaEdges.init(work_arena, group);
    var nfa_to_dfa: std.ArrayHashMapUnmanaged([]const usize, void, set.HashCtx, true) = .empty;
    const main_digraph = group.digraphs[0];

    var arena_instance: std.heap.ArenaAllocator = .init(gpa);
    errdefer arena_instance.deinit();
    const arena = arena_instance.allocator();
    var dfa_states: std.MultiArrayList(State) = .{};
    errdefer dfa_states.deinit(gpa);

    for (main_digraph.state_index..main_digraph.state_index + main_digraph.ir_count) |state| {
        try nfa_edges.state_set.append(work_arena, state);
    }
    for (main_digraph.state_index + main_digraph.ir_count + main_digraph.nr_count..main_digraph.state_index + main_digraph.ir_count + main_digraph.nr_count + main_digraph.ia_count) |state| {
        try nfa_edges.state_set.append(work_arena, state);
    }

    try nfa_edges.eClosure(work_arena, group);
    try nfa_to_dfa.putNoClobber(work_arena, try nfa_edges.state_set.toOwnedSlice(work_arena), {});

    var from_edges: std.ArrayListUnmanaged(usize) = .empty;
    while (dfa_states.len < nfa_to_dfa.count()) {
        const from_state = dfa_states.len;
        try dfa_states.append(gpa, .init(false));
        const slice = dfa_states.slice();
        const from_edge_mask = &slice.items(.edge_mask)[from_state];
        from_edges.clearRetainingCapacity();
        for (0..128) |symbol| {
            const from_state_set = nfa_to_dfa.entries.items(.key)[from_state];
            try nfa_edges.move(work_arena, from_state_set, @intCast(symbol));

            if (nfa_edges.state_set.items.len != 0) {
                try nfa_edges.eClosure(work_arena, group);

                const gop = try nfa_to_dfa.getOrPut(work_arena, nfa_edges.state_set.items);
                if (!gop.found_existing) {
                    gop.key_ptr.* = try nfa_edges.state_set.toOwnedSlice(work_arena);
                }
                from_edge_mask.* |= @as(u128, 1) << @intCast(symbol);
                try from_edges.append(work_arena, gop.index);
            }
        }
        slice.items(.edges)[from_state] = try arena.dupe(usize, from_edges.items);
        for (group.gate_edges) |gate_edge| {
            if (gate_edge.deferred) {
                const from_state_set = nfa_to_dfa.entries.items(.key)[from_state];
                const found, _ = set.find(from_state_set, gate_edge.from);
                if (found) {
                    nfa_edges.state_set.clearRetainingCapacity();
                    try nfa_edges.state_set.appendSlice(work_arena, from_state_set);
                    if (try set.insert(work_arena, &nfa_edges.state_set, gate_edge.to)) {
                        try nfa_edges.eClosure(work_arena, group);
                        const gop = try nfa_to_dfa.getOrPut(work_arena, nfa_edges.state_set.items);
                        if (!gop.found_existing) {
                            gop.key_ptr.* = try nfa_edges.state_set.toOwnedSlice(work_arena);
                        }
                        try deferred_gates.append(gpa, .{ .from = from_state, .to = gop.index, .digraph = gate_edge.digraph, .sign = gate_edge.sign, .deferred = false });
                    }
                }
            }
        }
    }

    const state_sets = nfa_to_dfa.entries.items(.key);
    const final = dfa_states.items(.final);
    for (0..nfa_to_dfa.count()) |i| {
        _, const final_state = set.find(state_sets[i], main_digraph.accept_begin());
        final[i] = final_state < state_sets[i].len and state_sets[i][final_state] < main_digraph.accept_end();
    }

    return .{ .{ .arena = arena_instance.state, .states = dfa_states }, try deferred_gates.toOwnedSlice(gpa) };
}

pub fn traverseEdge(edge_mask: u128, edges: []usize, ch: u7) ?usize {
    const bit = @as(u128, 1) << ch;
    if (edge_mask & bit != 0) {
        return edges[@popCount(edge_mask & (bit - 1))];
    } else {
        return null;
    }
}

pub fn deinit(dfa: *Dfa, gpa: std.mem.Allocator) void {
    dfa.arena.promote(gpa).deinit();
    dfa.states.deinit(gpa);
}

pub fn viz(dfa: Dfa, writer: anytype) !void {
    const slice = dfa.states.slice();
    const state_final = slice.items(.final);
    const state_edge_mask = slice.items(.edge_mask);
    const state_edges = slice.items(.edges);

    try writer.print("digraph{{node[shape=circle]", .{});
    for (0..dfa.states.len) |i| {
        if (!state_final[i]) {
            try writer.print(" {}", .{i});
        }
    }
    try writer.print(" node[shape=doublecircle]", .{});
    for (0..dfa.states.len) |i| {
        if (state_final[i]) {
            try writer.print(" {}", .{i});
        }
    }

    for (state_edges, 0..) |edges, from| {
        var labeled_edges: [128]struct { to: usize, mask: u128 } = undefined;
        var labeled_edge_count: usize = 0;

        var edges_it = edges;
        for (0..128) |symbol| {
            const bit = @as(u128, 1) << @intCast(symbol);
            if (state_edge_mask[from] & bit != 0) {
                const to = edges_it[0];
                defer edges_it = edges_it[1..];
                const index = for (labeled_edges[0..labeled_edge_count], 0..) |labeled_edge, index| {
                    if (labeled_edge.to == to) break index;
                } else index: {
                    defer labeled_edge_count += 1;
                    labeled_edges[labeled_edge_count].to = to;
                    labeled_edges[labeled_edge_count].mask = 0;
                    break :index labeled_edge_count;
                };
                labeled_edges[index].mask |= @as(u128, 1) << @intCast(symbol);
            }
        }
        for (labeled_edges[0..labeled_edge_count]) |edge| {
            try writer.print(" {}->{}[label=\"", .{ from, edge.to });
            try printMask(writer, edge.mask);
            try writer.print("\"]", .{});
        }
    }
    try writer.print("}}", .{});
}

pub fn format(dfa: Dfa, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
    _ = .{ fmt, options };
    try dfa.viz(writer);
}
