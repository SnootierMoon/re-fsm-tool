const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.resolveTargetQuery(.{
        .cpu_arch = .wasm32,
        .os_tag = .freestanding
    });
    const optimize = b.standardOptimizeOption(.{});
    const exe = b.addExecutable(.{
        .name = "librefsm",
        .root_source_file = b.path("src/exports.zig"),
        .target = target,
        .optimize = optimize,
    });
    exe.entry = .disabled;
    exe.rdynamic = true;
    b.installArtifact(exe);

    b.installFile("src/index.html", "index.html");
    b.installFile("src/styles.css", "styles.css");
    b.installFile("src/app.js", "app.js");

    const exe_unit_tests = b.addTest(.{
        .root_source_file = b.path("src/refsm.zig"),
        .target = target,
        .optimize = optimize,
    });
    const run_exe_unit_tests = b.addRunArtifact(exe_unit_tests);
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_exe_unit_tests.step);

    const serve_step = b.addSystemCommand(&.{"python3", "-m", "http.server"});
    serve_step.setCwd(.{ .cwd_relative = b.install_prefix });
    serve_step.step.dependOn(b.getInstallStep());
    const run_step = b.step("run", "Run the server");
    run_step.dependOn(&serve_step.step);
}
