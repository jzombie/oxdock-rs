#[cfg(not(miri))]
use libtest_mimic::Arguments;
#[cfg(not(miri))]
use oxdock_fs::PathResolver;
#[cfg(not(miri))]
use oxdock_logic_tests::harness::{
    HarnessConfig, build_trials, precompile_fixtures, prefer_tmpfs_for_tempdirs,
    resolve_shared_target_dir,
};

#[cfg(miri)]
fn main() {
    eprintln!(
        "Skipping integration fixture harness under Miri: requires cargo execution and fixture filesystem access."
    );
}

/// Fixtures whose trials spawn a binary built once up front (Tier 2).
///
/// These fixtures execute self-contained logic (already in-process DSL runs
/// inside their own `main`); pre-compiling once removes the per-trial
/// `cargo run` compile. Fixtures that test Cargo itself (`cargo check` /
/// `cargo test`, build scripts, proc-macros, per-case features) stay on the
/// Tier-3 per-trial cargo path and must NOT be listed here.
#[cfg(not(miri))]
const PRECOMPILED_FIXTURES: &[&str] = &[
    "copy_from_workspace",
    "copy_from_workspace_outside_escape",
    "copy_git_include_dirty",
];

#[cfg(not(miri))]
fn main() {
    // Process-global setup first: must precede the libtest-mimic worker pool.
    prefer_tmpfs_for_tempdirs();

    // No `test_threads` override: trials are independent (isolated tempdirs,
    // per-child env, no process-global mutation), so they run in parallel.
    // Cargo invocations share one target dir; Cargo serializes shared-target
    // access internally via its own locks.
    let args = Arguments::from_args();

    let resolver = PathResolver::from_manifest_env().unwrap_or_else(|err| {
        eprintln!("fixture harness failed to resolve manifest dir: {err:#}");
        std::process::exit(1);
    });

    let fixtures_root = resolver.root().join("fixtures").unwrap_or_else(|err| {
        eprintln!("fixture harness failed to resolve fixtures root: {err:#}");
        std::process::exit(1);
    });

    // Persistent shared target dir for every Tier-3 `cargo` trial: fixture
    // dependency artifacts compile once and are reused across runs (not just
    // within one run), so iteration pays cold builds exactly once ever.
    // `cargo clean` wipes it with the rest of target/.
    let (shared_target, target_keepalive) = resolve_shared_target_dir().unwrap_or_else(|err| {
        eprintln!("fixture harness failed to resolve shared target dir: {err:#}");
        std::process::exit(1);
    });

    // Tier-2: build each precompiled fixture once, before threads spawn.
    // The guards must stay alive until the run completes (they own the
    // instantiated fixture copies the binaries were built from).
    let prebuilt = precompile_fixtures(&fixtures_root, PRECOMPILED_FIXTURES, &shared_target, false)
        .unwrap_or_else(|err| {
            eprintln!("fixture harness pre-compilation failed: {err:#}");
            std::process::exit(1);
        });

    let mut config = HarnessConfig::new("integration", fixtures_root);
    config.exclude_root_dirs.push("commands".to_string());
    config.set_temp_target_dir = true;
    config.shared_target_dir = Some(shared_target);
    for fixture in &prebuilt {
        config
            .precompiled_binaries
            .insert(fixture.name.clone(), fixture.binary.clone());
    }

    let tests = build_trials(&resolver, &config).unwrap_or_else(|err| {
        eprintln!("fixture harness failed to discover fixtures: {err:#}");
        std::process::exit(1);
    });

    let result = libtest_mimic::run(&args, tests);
    drop(prebuilt);
    drop(target_keepalive);
    result.exit();
}
