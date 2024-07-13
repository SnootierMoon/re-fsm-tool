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

    fn insert(gpa: std.mem.Allocator, items: *std.ArrayListUnmanaged(usize), key: usize) !bool {
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

    fn init(gpa: std.mem.Allocator, edges: []Nfa.Edge) !NfaEdges {
        var map = NfaEdgeMap{};
        for (edges) |edge| {
            const entry = try map.getOrPutValue(gpa, .{ edge.from, edge.sym }, .{});
            try entry.value_ptr.append(gpa, edge.to);
        }

        return .{ .map = map, .pq = EClosurePriorityQueue.init(gpa, {}) };
    }

    fn eClosure(nfa_edges: *NfaEdges, gpa: std.mem.Allocator) !void {
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

    fn move(nfa_edges: *NfaEdges, gpa: std.mem.Allocator, from: []const usize, sym: u7) !void {
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

pub fn fromNfa(gpa: std.mem.Allocator, nfa: Nfa) !Dfa {
    var arena_obj = std.heap.ArenaAllocator.init(gpa);
    defer arena_obj.deinit();
    const arena = arena_obj.allocator();

    var nfa_edges = try NfaEdges.init(arena, nfa.edges);

    var nfa_to_dfa: std.ArrayHashMapUnmanaged([]const usize, void, set.HashCtx, true) = .{};
    var dfa_states: std.MultiArrayList(State) = .{};
    errdefer dfa_states.deinit(gpa);

    try nfa_edges.state_set.append(arena, 0);
    try nfa_edges.eClosure(arena);
    try nfa_to_dfa.putNoClobber(arena, try nfa_edges.state_set.toOwnedSlice(arena), {});

    while (dfa_states.len < nfa_to_dfa.count()) {
        const state = try dfa_states.addOne(gpa);
        for (0..128) |sym| {
            const slice = dfa_states.slice();
            const from_state_set = nfa_to_dfa.entries.items(.key)[state];
            const from_edges = &slice.items(.edges)[state];

            try nfa_edges.move(arena, from_state_set, @intCast(sym));
            try nfa_edges.eClosure(arena);

            const gop = try nfa_to_dfa.getOrPut(arena, nfa_edges.state_set.items);
            if (!gop.found_existing) {
                gop.key_ptr.* = try nfa_edges.state_set.toOwnedSlice(arena);
            }
            from_edges[sym] = gop.index;
        }
    }

    const state_sets = nfa_to_dfa.entries.items(.key);
    const finals = dfa_states.items(.final);
    for (0..nfa_to_dfa.count()) |i| {
        finals[i] = state_sets[i].len != 0 and state_sets[i][state_sets[i].len - 1] >= nfa.reject_count;
    }

    return .{ .states = dfa_states };
}

pub fn deinit(dfa: *Dfa, gpa: std.mem.Allocator) void {
    dfa.states.deinit(gpa);
}

pub fn minimize(dfa: Dfa, gpa: std.mem.Allocator) !Dfa {
    const slice = dfa.states.slice();
    const state_finals = slice.items(.final);
    const state_edges = slice.items(.edges);

    std.log.info("{any}", .{state_finals});

    for (state_finals[1..]) |final| {
        if (final != state_finals[0]) {
            break;
        }
    } else {
        var new_dfa_states: std.MultiArrayList(State) = .{};
        errdefer new_dfa_states.deinit(gpa);
        try new_dfa_states.append(gpa, .{
            .final = state_finals[0],
            .edges = .{0} ** 128,
        });
    
        return .{ .states = new_dfa_states };
    }

    var states_ordered = try gpa.alloc(usize, dfa.states.len);
    defer gpa.free(states_ordered);

    var state_partitions = try gpa.alloc(usize, dfa.states.len);
    defer gpa.free(state_partitions);

    var p = std.ArrayList([]usize).init(gpa);
    defer p.deinit();
    {
        var lo: usize = 1;
        var hi: usize = dfa.states.len;
        states_ordered[0] = 0; // if putting final states first, revert this!
        state_partitions[0] = 0;
        for (1..dfa.states.len) |state| {
            if (state_finals[state] == state_finals[0]) {
                states_ordered[lo] = state;
                lo += 1;
                state_partitions[state] = 0;
            } else {
                hi -= 1;
                states_ordered[hi] = state;
                state_partitions[state] = 1;
            }
        }
        try p.appendSlice(&.{ states_ordered[0..lo], states_ordered[lo..] });
    }

    std.log.info("{any}", .{p.items});
    var part_A: usize = 0;
    while (part_A < p.items.len) : (part_A += 1) {
        for (0..128) |ch| {
            for (0..p.items.len) |part_Y| {
                const part_0 = state_partitions[state_edges[p.items[part_Y][0]][ch]] == part_A;
                var lo: usize = 1;
                var hi = p.items[part_Y].len;
                while (lo < hi) {
                    const part_i = state_partitions[state_edges[p.items[part_Y][lo]][ch]] == part_A;
                    if (part_0 != part_i) {
                        hi -= 1;
                        std.mem.swap(usize, &p.items[part_Y][lo], &p.items[part_Y][hi]);
                    } else {
                        lo += 1;
                    }
                }
                if (hi != p.items[part_Y].len) {
                    if (lo > p.items[part_Y].len / 2) {
                        try p.append(p.items[part_Y][lo..]);
                        p.items[part_Y] = p.items[part_Y][0..lo];
                    } else {
                        try p.append(p.items[part_Y][0..lo]);
                        p.items[part_Y] = p.items[part_Y][lo..];
                    }
                    for (p.items[p.items.len - 1]) |index| {
                        state_partitions[index] = p.items.len - 1;
                    }
                }
            }
        }
        std.log.info("{any}", .{p.items});
    }

    var new_dfa_states: std.MultiArrayList(State) = .{};
    errdefer new_dfa_states.deinit(gpa);
    try new_dfa_states.resize(gpa, p.items.len);

    const new_slice = new_dfa_states.slice();
    const new_state_finals = new_slice.items(.final);
    const new_state_edges = new_slice.items(.edges);

    for (p.items, 0..) |part, new_state| {
        new_state_finals[new_state] = state_finals[part[0]];
        for (0..128) |sym| {
            new_state_edges[new_state][sym] = state_partitions[state_edges[part[0]][sym]];
        }
    }

    return .{ .states = new_dfa_states };
}

pub fn viz(dfa: Dfa, writer: anytype) !void {
    const slice = dfa.states.slice();
    const state_finals = slice.items(.final);
    const state_edges = slice.items(.edges);

    try writer.print("digraph {{", .{});
    try writer.print(" node [shape=circle]", .{});
    for (0..dfa.states.len) |i| {
        if (!state_finals[i]) {
            try writer.print(" {}\n", .{i});
        }
    }
    try writer.print(" node [shape=doublecircle]", .{});
    for (0..dfa.states.len) |i| {
        if (state_finals[i]) {
            try writer.print(" {}\n", .{i});
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
            try writer.print(" {} -> {} [label=\"", .{ from, edge.to });
            try @import("../re_fsm.zig").printMask(writer, edge.mask);
            try writer.print("\"]", .{});
        }
    }
    try writer.print(" }}", .{});
}
