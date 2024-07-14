const std = @import("std");

const Nfa = @import("Nfa.zig");
const Dfa = @import("Dfa.zig");

states: std.MultiArrayList(State),

const State = struct {
    final: bool,
    edges: [128]usize,
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
    sym_map: NfaSymEdgeMap,
    eps_map: NfaEpsEdgeMap,
    state_set: std.ArrayListUnmanaged(usize) = .{},
    pq: EpsClosurePriorityQueue,

    const EpsClosurePriorityQueue = std.PriorityQueue(usize, void, struct {
        fn f(ctx: void, lhs: usize, rhs: usize) std.math.Order {
            _ = ctx;
            return std.math.order(rhs, lhs);
        }
    }.f);

    const NfaSymEdgeMap = std.AutoHashMapUnmanaged(struct { usize, u7 }, std.ArrayListUnmanaged(usize));
    const NfaEpsEdgeMap = std.AutoHashMapUnmanaged(usize, struct { to: std.ArrayListUnmanaged(usize), gates: std.ArrayListUnmanaged(usize) });

    fn init(arena: std.mem.Allocator, group: Nfa.DigraphGroup) error{OutOfMemory}!NfaEdges {
        var sym_map = NfaSymEdgeMap{};
        var eps_map = NfaEpsEdgeMap{};
        for (group.edges.items) |edge| {
            if (edge.sym) |sym| {
                const entry = try sym_map.getOrPutValue(arena, .{ edge.from, sym }, .{});
                try entry.value_ptr.append(arena, edge.to);
            } else {
                const entry = try eps_map.getOrPutValue(arena, edge.from, .{ .to = .{}, .gates = .{} });
                try entry.value_ptr.to.append(arena, edge.to);
            }
        }
        for (group.gate_edges.items, 0..) |gate_edge, gate_edge_index| {
            const entry = try eps_map.getOrPutValue(arena, gate_edge.from, .{ .to = .{}, .gates = .{} });
            try entry.value_ptr.gates.append(arena, gate_edge_index);
        }

        return .{ .sym_map = sym_map, .eps_map = eps_map, .pq = EpsClosurePriorityQueue.init(arena, {}), };
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
                    const gate_edge = group.gate_edges.items[gate_edge_index];
                    const digraph = group.digraphs.items[gate_edge.digraph];
                    _, const final_state = set.find(nfa_edges.state_set.items, digraph.state_index + digraph.reject_count);
                    if ((final_state < nfa_edges.state_set.items.len and nfa_edges.state_set.items[final_state] < digraph.state_index + digraph.reject_count + digraph.accept_count) == (gate_edge.sign == .pos)) {
                        if (try set.insert(arena, &nfa_edges.state_set, gate_edge.to)) {
                            try nfa_edges.pq.add(gate_edge.to);
                        }
                    }
                }
            }
        }
    }

    fn move(nfa_edges: *NfaEdges, arena: std.mem.Allocator, from: []const usize, sym: u7) error{OutOfMemory}!void {
        nfa_edges.state_set.clearRetainingCapacity();
        for (from) |from_state| {
            if (nfa_edges.sym_map.get(.{ from_state, sym })) |to_states| {
                for (to_states.items) |to_state| {
                    _ = try set.insert(arena, &nfa_edges.state_set, to_state);
                }
            }
        }
    }
};

pub fn init(gpa: std.mem.Allocator, group: Nfa.DigraphGroup) error{OutOfMemory}!struct { Dfa, []Nfa.GateEdge } {
    var arena_obj = std.heap.ArenaAllocator.init(gpa);
    defer arena_obj.deinit();
    const arena = arena_obj.allocator();

    var deferred_gates: std.ArrayListUnmanaged(Nfa.GateEdge) = .{};
    defer deferred_gates.deinit(gpa);

    var nfa_edges = try NfaEdges.init(arena, group);

    var nfa_to_dfa: std.ArrayHashMapUnmanaged([]const usize, void, set.HashCtx, true) = .{};
    var dfa_states: std.MultiArrayList(State) = .{};
    errdefer dfa_states.deinit(gpa);

    try nfa_edges.state_set.append(arena, 0);
    try nfa_edges.eClosure(arena, group);
    try nfa_to_dfa.putNoClobber(arena, try nfa_edges.state_set.toOwnedSlice(arena), {});

    while (dfa_states.len < nfa_to_dfa.count()) {
        const state = try dfa_states.addOne(gpa);
        for (0..128) |sym| {
            const slice = dfa_states.slice();
            const from_state_set = nfa_to_dfa.entries.items(.key)[state];
            const from_edges = &slice.items(.edges)[state];

            try nfa_edges.move(arena, from_state_set, @intCast(sym));
            try nfa_edges.eClosure(arena, group);
            const gop = try nfa_to_dfa.getOrPut(arena, nfa_edges.state_set.items);
            if (!gop.found_existing) {
                gop.key_ptr.* = try nfa_edges.state_set.toOwnedSlice(arena);
            }

            from_edges[sym] = gop.index;
        }
        for (group.deferred_gate_edges.items) |gate_edge| {
            const from_state_set = nfa_to_dfa.entries.items(.key)[state];
            const found, _ = set.find(from_state_set, gate_edge.from);
            if (found) {
                nfa_edges.state_set.clearRetainingCapacity();
                try nfa_edges.state_set.appendSlice(gpa, from_state_set);
                if (try set.insert(arena, &nfa_edges.state_set, gate_edge.to)) {
                    try nfa_edges.eClosure(arena, group);
                    const gop = try nfa_to_dfa.getOrPut(arena, nfa_edges.state_set.items);
                    if (!gop.found_existing) {
                        gop.key_ptr.* = try nfa_edges.state_set.toOwnedSlice(arena);
                    }
                    try deferred_gates.append(gpa, .{ .from = state, .to = gop.index, .digraph = gate_edge.digraph, .sign = gate_edge.sign });
                }
            }
        }
    }

    const main_digraph = group.digraphs.items[0];
    const state_sets = nfa_to_dfa.entries.items(.key);
    const finals = dfa_states.items(.final);
    for (0..nfa_to_dfa.count()) |i| {
        _, const final_state = set.find(state_sets[i], main_digraph.state_index + main_digraph.reject_count);
        finals[i] = final_state < state_sets[i].len and state_sets[i][final_state] < main_digraph.state_index + main_digraph.reject_count + main_digraph.accept_count;
    }

    return .{ .{ .states = dfa_states }, try deferred_gates.toOwnedSlice(gpa) };
}

pub fn deinit(dfa: *Dfa, gpa: std.mem.Allocator) void {
    dfa.states.deinit(gpa);
}

pub fn viz(dfa: Dfa, writer: anytype) !void {
    const slice = dfa.states.slice();
    const state_finals = slice.items(.final);
    const state_edges = slice.items(.edges);

    try writer.print("digraph {{\n  node [shape=circle]\n ", .{});
    for (0..dfa.states.len) |i| {
        if (!state_finals[i]) {
            try writer.print(" {}", .{i});
        }
    }
    try writer.print("\n  node [shape=doublecircle]\n ", .{});
    for (0..dfa.states.len) |i| {
        if (state_finals[i]) {
            try writer.print(" {}", .{i});
        }
    }

    for (state_edges, 0..) |edges, from| {
        var labeled_edges: [128]struct{ to: usize, mask: u128 } = undefined;
        var labeled_edge_count: usize = 0;
        for (edges, 0..) |to, sym| {
            const index = for (labeled_edges[0..labeled_edge_count], 0..) |labeled_edge, index| {
                if (labeled_edge.to == to) break index;
            } else blk: {
                defer labeled_edge_count += 1;
                labeled_edges[labeled_edge_count].to = to;
                labeled_edges[labeled_edge_count].mask = 0;
                break :blk labeled_edge_count;
            };
            labeled_edges[index].mask |= @as(u128, 1) << @intCast(sym);
        }
        for (labeled_edges[0..labeled_edge_count]) |edge| {
            try writer.print("\n  {} -> {} [label=\"", .{ from, edge.to });
            try @import("../re_fsm.zig").printMask(writer, edge.mask);
            try writer.print("\"]", .{});
        }
    }
    try writer.print("\n}}", .{});
}
