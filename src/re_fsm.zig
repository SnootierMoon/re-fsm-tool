const std = @import("std");
const Ast = @import("re_fsm/Ast.zig");
const Nfa = @import("re_fsm/Nfa.zig");
const Dfa = @import("re_fsm/Dfa.zig");

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

pub fn run(writer: anytype, gpa: std.mem.Allocator, input_regex: []const u8) !void {
    var ast = try Ast.parse(gpa, input_regex);
    defer ast.deinit(gpa);

    var nfa = try Nfa.init(gpa, ast);
    defer nfa.deinit(gpa);

    var rev_group: Nfa.DigraphGroup = .{
        .total_states = 0,
        .edges = .{},
    };
    defer rev_group.deinit(gpa);
    try rev_group.digraphs.resize(gpa, nfa.groups.items.len);

    for (nfa.groups.items, 0..) |group, group_index| {
        var dfa, const gate_edges = try Dfa.init(gpa, group);
        defer dfa.deinit(gpa);
        defer gpa.free(gate_edges);

        const slice = dfa.states.slice();
        const state_final = slice.items(.final);
        const state_edge_mask = slice.items(.edge_mask);
        const state_edges = slice.items(.edges);

        for (0..dfa.states.len) |from_state| {
            if (state_final[from_state]) {
                try rev_group.edges.append(gpa, .{ .from = rev_group.total_states, .to = rev_group.total_states + from_state });
            }
            for (0..128) |sym| {
                const to_state = Dfa.edgeTo(state_edge_mask[from_state], state_edges[from_state].items, @intCast(sym)) orelse continue;
                try rev_group.edges.append(gpa, .{ .from = rev_group.total_states + to_state, .to = rev_group.total_states + from_state, .sym = @intCast(sym) });
            }
        }
        try rev_group.edges.append(gpa, .{ .from = rev_group.total_states + 1, .to = rev_group.total_states + dfa.states.len });
        for (gate_edges) |gate_edge| {
            try rev_group.gate_edges.append(gpa, .{
                .from = rev_group.total_states + gate_edge.to,
                .to = rev_group.total_states + gate_edge.from,
                .digraph = gate_edge.digraph,
                .sign = gate_edge.sign,
            });
        }
        if (group_index != 0) {
            try rev_group.edges.append(gpa, .{ .from = 0, .to = rev_group.total_states });
            for (0..128) |sym| {
                try rev_group.edges.append(gpa, .{ .from = rev_group.total_states, .to = rev_group.total_states, .sym = @intCast(sym) });
            }
        }
        rev_group.digraphs.items[group_index] = .{ .state_index = rev_group.total_states, .reject_count = dfa.states.len, .accept_count = 1 };
        rev_group.total_states += dfa.states.len + 1;
    }

    var rev_dfa, _ = try Dfa.init(gpa, rev_group);
    defer rev_dfa.deinit(gpa);

    var group: Nfa.DigraphGroup = .{
        .total_states = rev_dfa.states.len + 1,
    };
    defer group.deinit(gpa);
    try group.digraphs.append(gpa, .{ .state_index = 0, .reject_count = rev_dfa.states.len, .accept_count = 1 });
    const slice = rev_dfa.states.slice();
    const state_final = slice.items(.final);
    const state_edge_mask = slice.items(.edge_mask);
    const state_edges = slice.items(.edges);
    try group.edges.append(gpa, .{ .from = 1, .to = rev_dfa.states.len });
    for (0..rev_dfa.states.len) |from_state| {
        if (state_final[from_state]) {
            try group.edges.append(gpa, .{ .from = 0, .to = from_state });
        }
        for (0..128) |sym| {
            const to_state = Dfa.edgeTo(state_edge_mask[from_state], state_edges[from_state].items, @intCast(sym)) orelse continue;
            try group.edges.append(gpa, .{ .from = to_state, .to = from_state, .sym = @intCast(sym) });
        }
    }

    var dfa, _ = try Dfa.init(gpa, group);
    defer dfa.deinit(gpa);

    try dfa.viz(writer);
}
