const std = @import("std");

const Nfa = @import("Nfa.zig");
const Dfa = @import("Dfa.zig");

states: std.MultiArrayList(State),

const State = struct {
    final: bool,
    // perf: add a u128 for "active" edges - ones that don't lead to {}, empty
    // set
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

    fn insert(arena: std.mem.Allocator, s: *std.ArrayListUnmanaged(usize), key: usize) !bool {
        const found, const idx = find(s.items, key);
        if (!found) {
            try s.insert(arena, idx, key);
            return true;
        }
        return false;
    }

    const HashCtx = struct {
        pub fn hash(ctx: HashCtx, k: []const usize) u32 {
            _ = ctx;
            return @truncate(std.hash.Wyhash.hash(0, std.mem.sliceAsBytes(k)));
        }
        pub fn eql(ctx: HashCtx, k1: []const usize, k2: []const usize, k2_idx: usize) bool {
            _ = .{ ctx, k2_idx };
            return std.mem.eql(usize, k1, k2);
        }
    };
};

const NfaEdges = struct {
    map: Map,

    const Map = std.AutoHashMapUnmanaged(struct { usize, ?u7 }, std.ArrayListUnmanaged(usize));

    fn init(arena: std.mem.Allocator, edges: []Nfa.Edge) !NfaEdges {
        var map = Map{};
        for (edges) |edge| {
            const gop = try map.getOrPut(arena, .{ edge.from, edge.sym });
            if (!gop.found_existing) {
                gop.value_ptr.* = .{};
            }
            try gop.value_ptr.append(arena, edge.to);
        }

        return .{ .map = map };
    }

    fn eClosure(nfa_edges: *NfaEdges, arena: std.mem.Allocator, state_set: *std.ArrayListUnmanaged(usize)) !void {
        var pq = std.PriorityQueue(usize, void, struct {
            fn f(ctx: void, lhs: usize, rhs: usize) std.math.Order {
                _ = ctx;
                return std.math.order(rhs, lhs);
            }
        }.f).init(arena, {});
        try pq.addSlice(state_set.items);
        while (pq.removeOrNull()) |state| {
            if (nfa_edges.map.get(.{ state, null })) |to_list| {
                for (to_list.items) |to| {
                    if (try set.insert(arena, state_set, to)) {
                        try pq.add(to);
                    }
                }
            }
        }
    }

    fn move(nfa_edges: NfaEdges, arena: std.mem.Allocator, state_set: []const usize, sym: u7) !std.ArrayListUnmanaged(usize) {
        var new_state_set: std.ArrayListUnmanaged(usize) = .{};
        for (state_set) |state| {
            if (nfa_edges.map.get(.{ state, sym })) |to_list| {
                for (to_list.items) |to| {
                    _ = try set.insert(arena, &new_state_set, to);
                }
            }
        }
        return new_state_set;
    }
};

pub fn fromNfa(gpa: std.mem.Allocator, nfa: Nfa) !Dfa {
    var arena_obj = std.heap.ArenaAllocator.init(gpa);
    defer arena_obj.deinit();
    const arena = arena_obj.allocator();

    var nfa_edges = try NfaEdges.init(arena, nfa.edges);
    var nfa_to_dfa: std.ArrayHashMapUnmanaged([]const usize, void, set.HashCtx, true) = .{};

    var dfa_states: std.MultiArrayList(State) = .{};

    var dfa_state0_set: std.ArrayListUnmanaged(usize) = .{};
    try dfa_state0_set.append(arena, 0);
    try nfa_edges.eClosure(arena, &dfa_state0_set);
    try nfa_to_dfa.putNoClobber(arena, try dfa_state0_set.toOwnedSlice(arena), {});

    var curr_idx: usize = 0;
    while (curr_idx < nfa_to_dfa.count()) : (curr_idx += 1) {
        _ = try dfa_states.addOne(gpa);
        for (0..128) |sym| {
            var new_state_set = try nfa_edges.move(arena, nfa_to_dfa.entries.items(.key)[curr_idx], @intCast(sym));
            try nfa_edges.eClosure(arena, &new_state_set);

            const gop = try nfa_to_dfa.getOrPut(arena, try new_state_set.toOwnedSlice(arena));
            dfa_states.items(.edges)[curr_idx][sym] = gop.index;
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
    std.log.debug("{any}", .{dfa.states.items(.final)});
    dfa.states.deinit(gpa);
}
