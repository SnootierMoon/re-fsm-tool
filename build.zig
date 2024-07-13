const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const port = b.option(u16, "port", "port on which to run http.server");

    const wasm = b.addExecutable(.{
        .name = "ReFsm",
        .root_source_file = b.path("src/wasm_root.zig"),
        .target = b.resolveTargetQuery(.{ .cpu_arch = .wasm32, .os_tag = .freestanding }),
        .optimize = optimize,
    });
    wasm.entry = .disabled;
    wasm.rdynamic = true;

    const install_wasm = b.addInstallArtifact(wasm, .{ .dest_dir = .{ .override = .{ .custom = "www" } } });
    const install_extra_site_files = b.addInstallDirectory(.{
        .source_dir = b.path("src"),
        .install_dir = .{ .custom = "www" },
        .include_extensions = &.{ "css", "html", "ico", "js" },
        .install_subdir = "",
    });

    const install_website = b.step("install-www", "Install website files");
    install_website.dependOn(&install_wasm.step);
    install_website.dependOn(&install_extra_site_files.step);

    const http_server_step = b.addSystemCommand(&.{ "python3", "-m", "http.server", "-dwww" });
    if (port) |p| {
        http_server_step.addArg(b.fmt("{}", .{p}));
    }
    http_server_step.setCwd(.{ .cwd_relative = b.install_prefix });
    http_server_step.step.dependOn(install_website);

    const run_server_step = b.step("run-server", "Run the server");
    run_server_step.dependOn(&http_server_step.step);

    const cli = b.addExecutable(.{
        .name = "re_fsm",
        .root_source_file = b.path("src/cli.zig"),
        .target = target,
        .optimize = optimize,
    });

    const run_cli = b.addRunArtifact(cli);
    const run_cli_step = b.step("run-cli", "Run the command-line interface");
    run_cli_step.dependOn(&run_cli.step);

    const unit_tests = b.addTest(.{
        .root_source_file = b.path("src/re_fsm.zig"),
        .target = target,
        .optimize = optimize,
    });

    const run_unit_tests = b.addRunArtifact(unit_tests);
    const run_unit_tests_step = b.step("test", "Run unit tests");
    run_unit_tests_step.dependOn(&run_unit_tests.step);
}
