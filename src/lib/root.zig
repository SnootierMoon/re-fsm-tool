const std = @import("std");

pub const Ast = @import("Ast.zig");
pub const Dfa = @import("Dfa.zig");
pub const Nfa = @import("Nfa.zig");

pub const Flavor = enum(u8) {
    posix_bre,
    posix_ere,
    cmsc330,
    pcre,

    pub const values = std.enums.values(Flavor);
    pub const default: Flavor = .posix_bre;

    pub const infos: [values.len]Info = infos: {
        var infos_arr: [values.len]Info = undefined;
        for (&infos_arr, Flavor.values) |*info_ptr, flavor| {
            info_ptr.* = flavor.info();
        }
        break :infos infos_arr;
    };

    pub fn info(flavor: Flavor) Info {
        const desc = switch (flavor) {
            .posix_bre => "BRE (POSIX)",
            .posix_ere => "ERE (POSIX)",
            .cmsc330 => "CMSC330",
            .pcre => "PCRE (Perl)",
        };
        const example = switch (flavor) {
            .posix_bre => "^\\(abc\\+\\|xyz\\?\\)[0-3]*$",
            .posix_ere => "^(abc+|xyz?)[0-3]*$",
            .cmsc330 => "(abcc*|xy(z|))(0|1|2|3)*",
            .pcre => "unsupported!",
        };
        return .{
            .name = @tagName(flavor),
            .desc = desc,
            .example = example,
            .value = @intFromEnum(flavor),
        };
    }

    pub const Info = struct {
        name: [:0]const u8,
        desc: [:0]const u8,
        value: @typeInfo(Flavor).@"enum".tag_type,
        example: [:0]const u8,

        pub const names: [values.len][:0]const u8 = names: {
            var names_arr: [Flavor.values.len][:0]const u8 = undefined;
            for (&names_arr, infos) |*name, info_val| {
                name.* = info_val.name;
            }
            break :names names_arr;
        };

        pub const descs: [values.len][:0]const u8 = descs: {
            var descs_arr: [Flavor.values.len][:0]const u8 = undefined;
            for (&descs_arr, infos) |*desc, info_val| {
                desc.* = info_val.desc;
            }
            break :descs descs_arr;
        };
    };
};

fn constructReverseDigraph(
    dfa_arena: std.mem.Allocator,
    nfa_arena: std.mem.Allocator,
    group_in: Nfa.DigraphGroup,
    group_out: *Nfa.DigraphGroup,
    out_index: usize,
) error{OutOfMemory}!void {
    var dfa, const deferred_gate_edges = try Dfa.init(dfa_arena, group_in);
    const state_map = try dfa_arena.alloc(usize, dfa.states.len);

    const dfa_states = dfa.states.slice();
    const state_final = dfa_states.items(.final);
    const state_edge_mask = dfa_states.items(.edge_mask);
    const state_edges = dfa_states.items(.edges);

    const digraph = &group_out.digraphs[out_index];
    digraph.ir_count = 0;
    digraph.nr_count = 0;
    digraph.ia_count = @intFromBool(state_final[0]);
    digraph.na_count = @intFromBool(!state_final[0]);
    for (1..dfa.states.len) |state| {
        if (state_final[state]) {
            digraph.ir_count += 1;
        } else {
            digraph.nr_count += 1;
        }
    }
    group_out.allocateStatesFor(digraph);

    var ir_count: usize = 0;
    var nr_count: usize = 0;
    if (state_final[0]) {
        state_map[0] = digraph.ia_begin();
    } else {
        state_map[0] = digraph.na_begin();
    }
    for (1..dfa.states.len) |state| {
        if (state_final[state]) {
            state_map[state] = digraph.ir_begin() + ir_count;
            ir_count += 1;
        } else {
            state_map[state] = digraph.nr_begin() + nr_count;
            nr_count += 1;
        }
    }

    var new_edge_count: usize = 0;
    for (state_edge_mask) |edge_mask| {
        new_edge_count += @popCount(edge_mask);
    }

    const new_edges = try nfa_arena.realloc(group_out.edges, group_out.edges.len + new_edge_count);
    new_edge_count = group_out.edges.len;
    group_out.edges = new_edges;

    for (state_map, state_edge_mask, state_edges) |from_state, from_edge_mask, from_edges| {
        for (0..128) |symbol| {
            const to_state = state_map[Dfa.traverseEdge(from_edge_mask, from_edges, @intCast(symbol)) orelse continue];
            new_edges[new_edge_count] = .{ .from = to_state, .to = from_state, .symbol = @intCast(symbol) };
            new_edge_count += 1;
        }
    }

    const new_gate_edges = try nfa_arena.realloc(group_out.gate_edges, group_out.gate_edges.len + deferred_gate_edges.len);
    group_out.gate_edges.ptr = new_gate_edges.ptr;
    for (deferred_gate_edges) |gate_edge| {
        group_out.addGateEdge(.{
            .from = state_map[gate_edge.to],
            .to = state_map[gate_edge.from],
            .digraph = gate_edge.digraph,
            .sign = gate_edge.sign,
            .deferred = gate_edge.deferred,
        });
    }
}

pub fn minimumDfa(gpa: std.mem.Allocator, nfa: Nfa) std.mem.Allocator.Error!Dfa {
    const dfas = try gpa.alloc(Dfa, nfa.groups.len);
    defer gpa.free(dfas);

    var nfa_arena_instance: std.heap.ArenaAllocator = .init(gpa);
    defer nfa_arena_instance.deinit();
    const nfa_arena = nfa_arena_instance.allocator();

    var dfa_arena_instance: std.heap.ArenaAllocator = .init(gpa);
    defer dfa_arena_instance.deinit();
    const dfa_arena = dfa_arena_instance.allocator();

    {
        var rev_group: Nfa.DigraphGroup = .{
            .total_states = 0,
            .digraphs = try nfa_arena.alloc(Nfa.Digraph, nfa.groups.len),
            .gate_edges = try nfa_arena.alloc(Nfa.GateEdge, nfa.groups.len - 1),
            .edges = &.{},
        };

        for (nfa.groups, 0..) |group, group_index| {
            _ = dfa_arena_instance.reset(.retain_capacity);
            try constructReverseDigraph(dfa_arena, nfa_arena, group, &rev_group, group_index);
        }

        var final_group: Nfa.DigraphGroup = .{
            .total_states = 0,
            .digraphs = try nfa_arena.alloc(Nfa.Digraph, 1),
            .gate_edges = &.{},
            .edges = &.{},
        };

        _ = dfa_arena_instance.reset(.retain_capacity);
        try constructReverseDigraph(dfa_arena, nfa_arena, rev_group, &final_group, 0);

        const dfa, _ = try Dfa.init(gpa, final_group);
        return dfa;
    }
    // TODO: if nfa.groups.len == 1, perform hopcroft's algorithm
}

pub fn printMask(writer: anytype, mask: u128) !void {
    std.debug.assert(mask != 0);
    if (mask == std.math.maxInt(u128)) {
        try writer.print(".", .{});
    } else {
        var first = true;
        var start: ?u8 = null;
        for (32..128) |i| {
            if (mask & (@as(u128, 1) << @intCast(i)) != 0) {
                if (start == null) {
                    start = @as(u8, @intCast(i));
                }
            } else if (start) |start_| {
                const end: u8 = @as(u8, @intCast(i)) - 1;
                try printRange(writer, first, start_, end);
                first = false;
                start = null;
            }
        }
        if (start) |start_| {
            try printRange(writer, first, start_, '~');
        }
    }
}

fn printRange(writer: anytype, first: bool, start: u8, end: u8) !void {
    if (!first) {
        try writer.print(",", .{});
    }
    if (start == '\'' or start == '\"') {
        try writer.print("'\\{c}'", .{start});
    } else if (start == '\\') {
        try writer.print("'\\\\{c}'", .{start});
    } else {
        try writer.print("'{c}'", .{start});
    }
    if (start != end) {
        try writer.print("-", .{});
        if (end == '\'' or end == '\"') {
            try writer.print("'\\{c}'", .{end});
        } else if (end == '\\') {
            try writer.print("'\\\\{c}'", .{end});
        } else {
            try writer.print("'{c}'", .{end});
        }
    }
}
