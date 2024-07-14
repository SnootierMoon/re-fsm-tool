const std = @import("std");

const Nfa = @import("Nfa.zig");
const Dfa = @import("Dfa.zig");

has_dump_state: bool,
states: std.MultiArrayList(State),

const State = struct {
    final: bool,
    edge_mask: u128,
    edges: std.ArrayListUnmanaged(usize),
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

    fn insert(gpa: std.mem.Allocator, items: *std.ArrayListUnmanaged(usize), key: usize) error{OutOfMemory}!bool {
        const found, const index = find(items.items, key);
        if (!found) {
            try items.insert(gpa, index, key);
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
    map: NfaEdgeMap,
    state_set: std.ArrayListUnmanaged(usize) = .{},
    pq: EClosurePriorityQueue,

    const EClosurePriorityQueue = std.PriorityQueue(usize, void, struct {
            fn f(ctx: void, lhs: usize, rhs: usize) std.math.Order {
                _ = ctx;
                return std.math.order(rhs, lhs);
            }
    }.f);

    const NfaEdgeMap = std.AutoHashMapUnmanaged(struct { usize, ?u7 }, std.ArrayListUnmanaged(usize));

    fn init(gpa: std.mem.Allocator, edges: []Nfa.Edge) error{OutOfMemory}!NfaEdges {
        var map = NfaEdgeMap{};
        for (edges) |edge| {
            const entry = try map.getOrPutValue(gpa, .{ edge.from, edge.sym }, .{});
            try entry.value_ptr.append(gpa, edge.to);
        }

        return .{ .map = map, .pq = EClosurePriorityQueue.init(gpa, {}) };
    }

    fn eClosure(nfa_edges: *NfaEdges, gpa: std.mem.Allocator) error{OutOfMemory}!void {
        // https://github.com/ziglang/zig/pull/20282
        nfa_edges.pq.items.len = 0;
        try nfa_edges.pq.addSlice(nfa_edges.state_set.items);
        while (nfa_edges.pq.removeOrNull()) |state| {
            if (nfa_edges.map.get(.{ state, null })) |to_states| {
                for (to_states.items) |to_state| {
                    if (try set.insert(gpa, &nfa_edges.state_set, to_state)) {
                        try nfa_edges.pq.add(to_state);
                    }
                }
            }
        }
    }

    fn move(nfa_edges: *NfaEdges, gpa: std.mem.Allocator, from: []const usize, sym: u7) error{OutOfMemory}!void {
        nfa_edges.state_set.clearRetainingCapacity();
        for (from) |from_state| {
            if (nfa_edges.map.get(.{ from_state, sym })) |to_states| {
                for (to_states.items) |to_state| {
                    _ = try set.insert(gpa, &nfa_edges.state_set, to_state);
                }
            }
        }
    }
};

pub fn fromNfa(gpa: std.mem.Allocator, nfa: Nfa) error{OutOfMemory}!Dfa {
    var arena_obj = std.heap.ArenaAllocator.init(gpa);
    defer arena_obj.deinit();
    const arena = arena_obj.allocator();

    var nfa_edges = try NfaEdges.init(arena, nfa.edges);
    var nfa_to_dfa: std.ArrayHashMapUnmanaged([]const usize, void, set.HashCtx, true) = .{};

    var has_dump_state = false;
    var dfa_states: std.MultiArrayList(State) = .{};
    errdefer {
        for (dfa_states.items(.edges)) |*edges| {
            edges.deinit(gpa);
        }
        dfa_states.deinit(gpa);
    }

    try nfa_to_dfa.putNoClobber(arena, &.{}, {});
    try dfa_states.append(gpa, .{ .final = false, .edge_mask = 0, .edges = .{} });

    try nfa_edges.state_set.append(arena, 0);
    try nfa_edges.eClosure(arena);
    try nfa_to_dfa.putNoClobber(arena, try nfa_edges.state_set.toOwnedSlice(arena), {});

    while (dfa_states.len < nfa_to_dfa.count()) {
        const state = dfa_states.len;
        try dfa_states.append(gpa, .{ .final = undefined, .edge_mask = 0, .edges = .{} });
        const slice = dfa_states.slice();
        const from_edge_mask = &slice.items(.edge_mask)[state];
        var from_edges = &slice.items(.edges)[state];
        for (0..128) |sym| {
            const from_state_set = nfa_to_dfa.entries.items(.key)[state];
            try nfa_edges.move(arena, from_state_set, @intCast(sym));

            if (nfa_edges.state_set.items.len == 0) {
                has_dump_state = true;
            } else {
                try nfa_edges.eClosure(arena);
    
                const gop = try nfa_to_dfa.getOrPut(arena, nfa_edges.state_set.items);
                if (!gop.found_existing) {
                    gop.key_ptr.* = try nfa_edges.state_set.toOwnedSlice(arena);
                }
                from_edge_mask.* |= @as(u128, 1) << @intCast(sym);
                try from_edges.append(gpa, gop.index);
            }
        }
    }

    const state_sets = nfa_to_dfa.entries.items(.key);
    const finals = dfa_states.items(.final);
    for (0..nfa_to_dfa.count()) |i| {
        finals[i] = state_sets[i].len != 0 and state_sets[i][state_sets[i].len - 1] >= nfa.reject_count;
    }

    return .{ .has_dump_state = has_dump_state, .states = dfa_states };
}

fn edgeTo(edge_mask: u128, edges: []usize, ch: u7) usize {
    const bit = @as(u128, 1) << ch;
    if (edge_mask & bit != 0) {
        return edges[@popCount(edge_mask & (bit - 1))];
    } else {
        return 0;
    }
}

pub fn deinit(dfa: *Dfa, gpa: std.mem.Allocator) void {
        for (dfa.states.items(.edges)) |*edges| {
            edges.deinit(gpa);
        }
    dfa.states.deinit(gpa);
}

pub fn minimize(dfa: Dfa, gpa: std.mem.Allocator) error{OutOfMemory}!Dfa {
    const slice = dfa.states.slice();
    const state_finals = slice.items(.final);
    const state_edge_mask = slice.items(.edge_mask);
    const state_edges = slice.items(.edges);

    const no_ds: usize = @intFromBool(!dfa.has_dump_state);

    for (state_finals[1 + no_ds..]) |final| {
        if (final != state_finals[no_ds]) {
            break;
        }
    } else {
        var new_dfa_states: std.MultiArrayList(State) = .{};
        errdefer new_dfa_states.deinit(gpa);
        var edges: std.ArrayListUnmanaged(usize) = .{};
        errdefer edges.deinit(gpa);
        try edges.appendNTimes(gpa, 1, 128);
        try new_dfa_states.append(gpa, .{ .final = false, .edge_mask = 0, .edges = .{} });
        try new_dfa_states.append(gpa, .{ .final = state_finals[no_ds], .edge_mask = std.math.maxInt(u128), .edges = edges });
        return .{ .has_dump_state = true, .states = new_dfa_states };
    }

    var states_ordered = try gpa.alloc(usize, dfa.states.len - no_ds);
    defer gpa.free(states_ordered);

    var state_partition = try gpa.alloc(usize, dfa.states.len - no_ds);
    defer gpa.free(state_partition);

    var p: std.ArrayListUnmanaged([]usize) = .{};
    defer p.deinit(gpa);
    {
        var lo: usize = no_ds + 1;
        var hi: usize = dfa.states.len;
        states_ordered[0] = no_ds; // if putting final states first, revert this!
        state_partition[0] = no_ds;
        for (1..states_ordered.len) |state| {
            if (state_finals[state] == state_finals[0]) {
                states_ordered[lo] = state;
                lo += 1;
                state_partition[state] = 0;
            } else {
                hi -= 1;
                states_ordered[hi] = state;
                state_partition[state] = 1;
            }
        }
        try p.appendSlice(gpa, &.{ states_ordered[0..lo], states_ordered[lo..] });
    }

    var part_A: usize = 0;
    while (part_A < p.items.len) : (part_A += 1) {
        for (0..128) |sym| {
            for (0..p.items.len) |part_Y| {
                const state_0 = p.items[part_Y][0];
                const part_0 = state_partition[edgeTo(state_edge_mask[state_0], state_edges[state_0].items, @intCast(sym))] == part_A;
                var lo: usize = 1;
                var hi = p.items[part_Y].len;
                while (lo < hi) {
                    const state_i = p.items[part_Y][lo];
                    const part_i = state_partition[edgeTo(state_edge_mask[state_i], state_edges[state_i].items, @intCast(sym))] == part_A;
                    if (part_0 != part_i) {
                        hi -= 1;
                        std.mem.swap(usize, &p.items[part_Y][lo], &p.items[part_Y][hi]);
                    } else {
                        lo += 1;
                    }
                }
                if (hi != p.items[part_Y].len) {
                    if (lo > p.items[part_Y].len / 2) {
                        try p.append(gpa, p.items[part_Y][lo..]);
                        p.items[part_Y] = p.items[part_Y][0..lo];
                    } else {
                        try p.append(gpa, p.items[part_Y][0..lo]);
                        p.items[part_Y] = p.items[part_Y][lo..];
                    }
                    for (p.items[p.items.len - 1]) |index| {
                        state_partition[index] = p.items.len - 1;
                    }
                }
            }
        }
    }

    var canon_state = try gpa.alloc(?usize, p.items.len);
    defer gpa.free(canon_state);
    @memset(canon_state, null);
    canon_state[state_partition[1]] = 1;
    const has_dump_state = for (p.items, 0..) |part, part_index| {
        const is_dump_state = for (0..128) |sym| {
            if (state_partition[edgeTo(state_edge_mask[part[0]], state_edges[part[0]].items, @intCast(sym))] != part_index) {
                break false;
            }
        } else true;

        if (is_dump_state) {
            canon_state[part_index] = 0;
            break true;
        }
    } else false;

    var new_dfa_states: std.MultiArrayList(State) = .{};
    errdefer new_dfa_states.deinit(gpa);
    try new_dfa_states.resize(gpa, @intFromBool(!has_dump_state) + p.items.len);

    const new_slice = new_dfa_states.slice();
    const new_state_finals = new_slice.items(.final);
    const new_state_edge_mask = new_slice.items(.edge_mask);
    const new_state_edges = new_slice.items(.edges);

    new_state_finals[0] = false;
    new_state_edge_mask[0] = 0;
    new_state_edges[0] = .{};

    var next_canon_state: usize = 2;
    var fifo = std.fifo.LinearFifo(usize, .Dynamic).init(gpa);
    defer fifo.deinit();
    try fifo.writeItem(state_partition[1]);

    while (fifo.readItem()) |part_index| {
        const part = p.items[part_index];
        const new_state = canon_state[part_index].?;
        new_state_finals[new_state] = state_finals[part[0]];
        new_state_edge_mask[new_state] = 0;
        new_state_edges[new_state] = .{};
        for (0..128) |sym| {
            const to_part_index = state_partition[edgeTo(state_edge_mask[part[0]], state_edges[part[0]].items, @intCast(sym))];
            if (canon_state[to_part_index] == null) {
                canon_state[to_part_index] = next_canon_state;
                next_canon_state += 1;
                try fifo.writeItem(to_part_index);
            }
            if (canon_state[to_part_index] != 0) {
                new_state_edge_mask[new_state] |= @as(u128, 1) << @intCast(sym);
                try new_state_edges[new_state].append(gpa, canon_state[to_part_index].?);
            }
        }
    }
    
    return .{ .has_dump_state = has_dump_state, .states = new_dfa_states };
}

pub fn viz(dfa: Dfa, writer: anytype) !void {
    const slice = dfa.states.slice();
    const state_finals = slice.items(.final);
    const state_edge_mask = slice.items(.edge_mask);
    const state_edges = slice.items(.edges);

    try writer.print("digraph {{\n  node [shape=circle]\n ", .{});
    for (1..dfa.states.len) |i| {
        if (!state_finals[i]) {
            try writer.print(" {}", .{i});
        }
    }
    try writer.print("\n  node [shape=doublecircle]\n ", .{});
    for (1..dfa.states.len) |i| {
        if (state_finals[i]) {
            try writer.print(" {}", .{i});
        }
    }

    for (state_edges, 0..) |edges, from| {
        var labeled_edges: [128]struct{ to: usize, mask: u128 } = undefined;
        var labeled_edge_count: usize = 0;

        var edges_it = edges.items;
        for (0..128) |sym| {
            const bit = @as(u128, 1) << @intCast(sym);
            if (state_edge_mask[from] & bit != 0) {
                const to = edges_it[0];
                defer edges_it = edges_it[1..];
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
        }
        for (labeled_edges[0..labeled_edge_count]) |edge| {
            try writer.print("\n  {} -> {} [label=\"", .{ from, edge.to });
            try @import("../re_fsm.zig").printMask(writer, edge.mask);
            try writer.print("\"]", .{});
        }
    }
    try writer.print("\n}}", .{});
}
