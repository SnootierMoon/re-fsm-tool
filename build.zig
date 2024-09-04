const std = @import("std");

const Flavor = @import("src/re-fsm/root.zig").Flavor;

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // Main library

    const re_fsm = b.createModule(.{
        .root_source_file = b.path("src/re-fsm/root.zig"),
    });

    const unit_tests = b.addTest(.{
        .root_source_file = b.path("src/re-fsm/tests.zig"),
    });
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&b.addRunArtifact(unit_tests).step);

    // Website files

    const wasm = b.addExecutable(.{
        .name = "re-fsm",
        .root_source_file = b.path("src/web/re-fsm.zig"),
        .target = b.resolveTargetQuery(.{
            .cpu_arch = .wasm32,
            .os_tag = .freestanding,
        }),
        .optimize = switch (optimize) {
            .ReleaseFast => .ReleaseSmall,
            else => optimize,
        },
    });
    wasm.rdynamic = true;
    wasm.entry = .disabled;
    wasm.root_module.addImport("re-fsm", re_fsm);

    const web_files = b.addWriteFiles();
    _ = web_files.addCopyDirectory(b.path("src/web"), ".", .{});
    _ = web_files.addCopyFile(wasm.getEmittedBin(), "re-fsm.wasm");
    const web_build = b.addWriteFiles();
    _ = web_build.addCopyDirectory(web_files.getDirectory(), "files", .{});
    const gen_index = b.addRunArtifact(b.addExecutable(.{
        .name = "gen-web-index",
        .root_source_file = b.path("build/gen-web-index.zig"),
        .target = b.graph.host,
    }));
    gen_index.addDirectoryArg(web_files.getDirectory());
    const web_index_zig = web_build.addCopyFile(gen_index.captureStdOut(), "index.zig");

    const install_web_files_step = b.step("install-web", "Install website files");
    install_web_files_step.dependOn(&gen_index.step);
    install_web_files_step.dependOn(&b.addInstallDirectory(.{
        .source_dir = web_files.getDirectory(),
        .install_dir = .{ .custom = "web" },
        .install_subdir = ".",
    }).step);

    // Command-line testing program

    const cli = b.addExecutable(.{
        .name = "re-fsm-cli",
        .root_source_file = b.path("src/bin/cli.zig"),
        .target = target,
        .optimize = optimize,
    });
    b.installArtifact(cli);
    cli.root_module.addImport("re-fsm", re_fsm);

    const cli_run_artifact = b.addRunArtifact(cli);
    const cli_run_step = b.step("run-cli", "Run the command-line interface");
    cli_run_step.dependOn(&cli_run_artifact.step);
    if (b.option(Flavor, "flavor", "the Regex flavor to use for the command-line interface")) |flavor| {
        cli_run_artifact.addArgs(&.{@tagName(flavor)});
    }

    // Server testing program

    const server = b.addExecutable(.{
        .name = "re-fsm-server",
        .root_source_file = b.path("src/bin/server.zig"),
        .target = target,
        .optimize = optimize,
    });
    b.installArtifact(server);
    server.root_module.addAnonymousImport(
        "web-build",
        .{ .root_source_file = web_index_zig },
    );
    server.step.dependOn(&gen_index.step);
    const server_run_artifact = b.addRunArtifact(server);
    const server_run_step = b.step("run-server", "Run the command-line interface");
    server_run_step.dependOn(&server_run_artifact.step);
    if (b.option(u16, "port", "the port to host http on")) |port| {
        server_run_artifact.addArgs(&.{b.fmt("{}", .{port})});
    }
}
