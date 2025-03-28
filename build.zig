const std = @import("std");

const Flavor = @import("src/lib/root.zig").Flavor;

const version = std.SemanticVersion.parse("0.1.0") catch unreachable;

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // main FSM library
    const lib_mod = b.createModule(.{
        .root_source_file = b.path("src/lib/root.zig"),
    });

    const unit_tests_mod = b.createModule(.{
        .root_source_file = b.path("src/lib/tests.zig"),
        .target = target,
        .optimize = optimize,
    });
    const unit_tests = b.addTest(.{
        .root_module = unit_tests_mod,
    });
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&b.addRunArtifact(unit_tests).step);

    const build_info_options = b.addOptions();
    build_info_options.addOption(std.SemanticVersion, "version", version);
    const build_info = build_info_options.createModule();

    // wasm compilation
    const wasm_mod = b.createModule(.{
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
    const wasm = b.addExecutable(.{
        .name = "re-fsm",
        .root_module = wasm_mod,
    });
    wasm.rdynamic = true;
    wasm.entry = .disabled;
    wasm_mod.addImport("re-fsm", lib_mod);
    wasm_mod.addImport("build-info", build_info);

    // contains all web files - handwritten and generated
    const web_dir = b.addWriteFiles();
    _ = web_dir.addCopyDirectory(b.path("src/web"), ".", .{
        .include_extensions = &.{ "css", "html", "ico", "js" },
    });
    _ = web_dir.addCopyFile(wasm.getEmittedBin(), "re-fsm.wasm");

    const install_web_dir_step = b.step("install-web", "Install website files");
    install_web_dir_step.dependOn(&b.addInstallDirectory(.{
        .source_dir = web_dir.getDirectory(),
        .install_dir = .prefix,
        .install_subdir = "web",
    }).step);

    // contains root of web module, i.e. ./index.zig, ./files/{web files}
    const web_mod_dir = b.addWriteFiles();
    _ = web_mod_dir.addCopyDirectory(web_dir.getDirectory(), "files", .{});
    const gen_index_mod = b.addModule("build-web-index", .{
        .root_source_file = b.path("build/web-index.zig"),
        .target = b.graph.host,
    });
    const gen_index = b.addRunArtifact(b.addExecutable(.{
        .name = "build-web-index",
        .root_module = gen_index_mod,
    }));
    gen_index.addDirectoryArg(web_dir.getDirectory());
    const web_mod = b.addModule("web", .{
        .root_source_file = web_mod_dir.addCopyFile(gen_index.captureStdOut(), "index.zig"),
        .target = target,
        .optimize = optimize,
    });

    // server testing program
    const server_mod = b.createModule(.{
        .root_source_file = b.path("src/bin/server.zig"),
        .target = target,
        .optimize = optimize,
    });
    const server = b.addExecutable(.{
        .name = "re-fsm-server",
        .root_module = server_mod,
    });
    server_mod.addImport("build-info", build_info);
    server_mod.addImport("web", web_mod);
    server.step.dependOn(&gen_index.step);

    b.installArtifact(server);
    const install_server_step = b.step("server", "Install the server");
    install_server_step.dependOn(&b.addInstallArtifact(server, .{}).step);

    const run_server = b.addRunArtifact(server);
    const run_server_step = b.step("run-server", "Run the server");
    run_server_step.dependOn(&run_server.step);
    if (b.option(u16, "port", "http server port")) |p| {
        run_server.addArgs(&.{ "-port", b.fmt("{}", .{p}) });
    }
    if (b.option(bool, "open", "open http page in browser")) |o| {
        if (o) {
            run_server.addArg("-open");
        }
    }

    // command-line testing program
    const cli_mod = b.createModule(.{
        .root_source_file = b.path("src/bin/cli.zig"),
        .target = target,
        .optimize = optimize,
    });
    const cli = b.addExecutable(.{
        .name = "re-fsm-cli",
        .root_module = cli_mod,
    });
    cli_mod.addImport("re-fsm", lib_mod);
    cli_mod.addImport("build-info", build_info);

    b.installArtifact(cli);
    const install_cli_step = b.step("cli", "Install the command-line interface");
    install_cli_step.dependOn(&b.addInstallArtifact(cli, .{}).step);

    const run_cli = b.addRunArtifact(cli);
    const run_cli_step = b.step("run-cli", "Run the command-line interface");
    run_cli_step.dependOn(&run_cli.step);
    if (b.option(Flavor, "flavor", "the Regex flavor to use for the command-line interface")) |flavor| {
        run_cli.addArgs(&.{@tagName(flavor)});
    }
}
