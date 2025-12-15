#![deny(clippy::disallowed_methods)]

pub mod ast;
pub mod exec;
pub use ast::*;
pub use exec::*;

#[cfg(test)]
#[allow(clippy::disallowed_methods)]
mod tests {
    use super::*;
    use indoc::indoc;
    use oxdock_fs::{GuardedPath, PathResolver};
    #[cfg(unix)]
    use std::time::Instant;

    fn guard_root(path: &std::path::Path) -> GuardedPath {
        GuardedPath::new_root(path).unwrap()
    }

    fn read_trimmed(path: &GuardedPath) -> String {
        let resolver = PathResolver::new(path.root(), path.root()).unwrap();
        resolver
            .read_to_string(path)
            .unwrap_or_default()
            .trim()
            .to_string()
    }

    fn create_dirs(path: &GuardedPath) {
        let resolver = PathResolver::new(path.root(), path.root()).unwrap();
        resolver.create_dir_all_abs(path).unwrap();
    }

    fn exists(root: &GuardedPath, rel: &str) -> bool {
        root.as_path().join(rel).exists()
    }

    #[test]
    fn run_sets_cargo_target_dir_to_fs_root() {
        let temp = tempfile::tempdir().unwrap();
        let root = guard_root(temp.path());

        let cmd = if cfg!(windows) {
            "echo %CARGO_TARGET_DIR% > seen.txt"
        } else {
            "printf %s \"$CARGO_TARGET_DIR\" > seen.txt"
        };

        let steps = vec![Step {
            guards: Vec::new(),
            kind: StepKind::Run(cmd.to_string()),
        }];

        run_steps(&root, &steps).unwrap();

        let seen = read_trimmed(&root.join("seen.txt").unwrap());
        let expected = root.join(".cargo-target").unwrap();
        assert_eq!(
            seen.trim(),
            expected.display().to_string(),
            "CARGO_TARGET_DIR should be scoped"
        );
    }

    #[test]
    fn guard_skips_when_env_missing() {
        let temp = tempfile::tempdir().unwrap();
        let root = guard_root(temp.path());

        let guard_var = "OXDOCK_GUARD_TEST_TOKEN_UNSET";
        let script = format!(
            indoc!(
                r#"
                [env:{guard}] WRITE skipped.txt hi
                WRITE kept.txt ok
                "#
            ),
            guard = guard_var
        );
        let steps = parse_script(&script).unwrap();

        run_steps(&root, &steps).unwrap();

        assert!(
            !exists(&root, "skipped.txt"),
            "guarded WRITE should be skipped"
        );
        assert!(exists(&root, "kept.txt"), "unguarded WRITE should run");
    }

    #[test]
    fn guard_sees_env_set_by_env_step() {
        let temp = tempfile::tempdir().unwrap();
        let root = guard_root(temp.path());

        let script = indoc!(
            r#"
            ENV FOO=1
            [env:FOO] WRITE hit.txt yes
            WRITE always.txt ok
            "#
        );
        let steps = parse_script(script).unwrap();

        run_steps(&root, &steps).unwrap();

        assert!(
            exists(&root, "hit.txt"),
            "guarded WRITE should run after ENV sets variable"
        );
        assert!(
            exists(&root, "always.txt"),
            "unguarded WRITE should run"
        );
    }

    #[test]
    fn echo_runs_and_allows_subsequent_steps() {
        let temp = tempfile::tempdir().unwrap();
        let root = guard_root(temp.path());

        let script = indoc!(
            r#"
            ECHO Hello, world
            WRITE always.txt ok
            "#
        );
        let steps = parse_script(script).unwrap();

        run_steps(&root, &steps).unwrap();

        assert!(
            exists(&root, "always.txt"),
            "WRITE after ECHO should run"
        );
    }

    #[test]
    fn guard_on_previous_line_applies_to_next_command() {
        let temp = tempfile::tempdir().unwrap();
        let root = guard_root(temp.path());

        let script = indoc!(
            r#"
            ENV FOO=1
            [env:FOO]
            WRITE hit.txt yes
            WRITE always.txt ok
            "#
        );
        let steps = parse_script(script).unwrap();

        run_steps(&root, &steps).unwrap();

        assert!(
            exists(&root, "hit.txt"),
            "guarded WRITE on next line should run"
        );
        assert!(
            exists(&root, "always.txt"),
            "unguarded WRITE should run"
        );
    }

    #[test]
    fn guard_respects_platform_negation() {
        let temp = tempfile::tempdir().unwrap();
        let root = guard_root(temp.path());

        let script = indoc!(
            r#"
            [!unix] WRITE platform.txt hi
            WRITE always.txt ok
            "#
        );
        let steps = parse_script(script).unwrap();

        run_steps(&root, &steps).unwrap();

        let expect_skipped = cfg!(unix);
        assert_eq!(
            exists(&root, "platform.txt"),
            !expect_skipped,
            "platform guard should skip on unix and run elsewhere"
        );
        assert!(
            exists(&root, "always.txt"),
            "unguarded WRITE should run"
        );
    }

    #[test]
    fn guard_matches_profile_env() {
        // Cargo sets PROFILE during builds/tests; verify guards see it.
        let temp = tempfile::tempdir().unwrap();
        let root = guard_root(temp.path());

        let profile = std::env::var("PROFILE").unwrap_or_else(|_| "debug".to_string());
        unsafe {
            std::env::set_var("PROFILE", &profile);
        }
        let script = format!(
            indoc!(
                r#"
                [env:PROFILE={0}] WRITE hit.txt yes
                [env:PROFILE!={0}] WRITE miss.txt no
                "#
            ),
            profile
        );

        let steps = parse_script(&script).unwrap();
        run_steps(&root, &steps).unwrap();

        assert!(
            exists(&root, "hit.txt"),
            "PROFILE-matching guard should run"
        );
        assert!(
            !exists(&root, "miss.txt"),
            "PROFILE inequality guard should skip for current profile"
        );
    }

    #[test]
    fn multiple_guards_all_must_pass() {
        let temp = tempfile::tempdir().unwrap();
        let root = guard_root(temp.path());

        let key = "OXDOCK_MULTI_GUARD_TEST_PASS";
        unsafe {
            std::env::set_var(key, "ok");
        }

        let script = format!(
            indoc!(
                r#"
                [env:{k},env:{k}=ok] WRITE hit.txt yes
                WRITE always.txt ok
                "#
            ),
            k = key
        );
        let steps = parse_script(&script).unwrap();
        run_steps(&root, &steps).unwrap();

        assert!(
            exists(&root, "hit.txt"),
            "guarded step should run when all guards pass"
        );
        assert!(
            exists(&root, "always.txt"),
            "unguarded step should run"
        );
    }

    #[test]
    fn multiple_guards_skip_when_one_fails() {
        let temp = tempfile::tempdir().unwrap();
        let root = guard_root(temp.path());

        let key = "OXDOCK_MULTI_GUARD_TEST_FAIL";
        unsafe {
            std::env::set_var(key, "ok");
        }

        let script = format!(
            indoc!(
                r#"
                [env:{k},env:{k}!=ok] WRITE miss.txt yes
                WRITE always.txt ok
                "#
            ),
            k = key
        );
        let steps = parse_script(&script).unwrap();
        run_steps(&root, &steps).unwrap();

        assert!(
            !exists(&root, "miss.txt"),
            "guarded step should skip when any guard fails"
        );
        assert!(
            exists(&root, "always.txt"),
            "unguarded step should run"
        );
    }

    #[cfg(unix)]
    #[test]
    fn run_bg_exits_success_and_stops_pipeline() {
        let temp = tempfile::tempdir().unwrap();
        let root = guard_root(temp.path());

        // Background succeeds quickly; pipeline should complete without error.
        let script = "RUN_BG sh -c 'sleep 0.05'";
        let steps = parse_script(script).unwrap();
        let res = run_steps(&root, &steps);
        assert!(res.is_ok(), "RUN_BG success should allow clean exit");
    }

    #[cfg(unix)]
    #[test]
    fn run_bg_failure_bubbles_status() {
        let temp = tempfile::tempdir().unwrap();
        let root = guard_root(temp.path());

        let script = "RUN_BG sh -c 'sleep 0.05; exit 7'";
        let steps = parse_script(script).unwrap();
        let err = run_steps(&root, &steps).unwrap_err();
        let msg = err.to_string();
        assert!(
            msg.contains("RUN_BG exited with status") || msg.contains("exit status: 7"),
            "should surface failing RUN_BG exit code"
        );
    }

    #[cfg(unix)]
    #[test]
    fn run_bg_multiple_stops_on_first_exit_and_does_not_block_steps() {
        let temp = tempfile::tempdir().unwrap();
        let root = guard_root(temp.path());

        let script = indoc! {
            r#"
            RUN_BG sh -c 'sleep 0.2; echo one > one.txt'
            RUN_BG sh -c 'sleep 0.5; echo two > two.txt'
            WRITE done.txt ok
            "#
        };

        let steps = parse_script(script).unwrap();
        let start = Instant::now();
        let res = run_steps(&root, &steps);
        let elapsed = start.elapsed();

        assert!(res.is_ok(), "RUN_BG success should allow clean exit");
        assert!(
            exists(&root, "done.txt"),
            "foreground step should run after spawning backgrounds"
        );
        assert!(
            exists(&root, "one.txt"),
            "first background should finish and emit output"
        );
        assert!(
            !exists(&root, "two.txt"),
            "second background should be terminated once the first exits"
        );
        assert!(
            elapsed.as_secs_f32() < 0.45 && elapsed.as_secs_f32() > 0.15,
            "should wait roughly for first background (~0.2s) but not the second (~0.5s); got {elapsed:?}"
        );
    }

    #[cfg(unix)]
    #[test]
    fn exit_terminates_backgrounds_and_returns_code() {
        let temp = tempfile::tempdir().unwrap();
        let root = guard_root(temp.path());

        let script = indoc! {
            r#"
            RUN_BG sh -c 'sleep 1; echo late > late.txt'
            EXIT 5
            "#
        };

        let steps = parse_script(script).unwrap();
        let err = run_steps(&root, &steps).unwrap_err();
        let msg = err.to_string();
        assert!(msg.contains("EXIT requested with code 5"));
        assert!(
            !exists(&root, "late.txt"),
            "background process should be killed when EXIT is hit"
        );
    }

    #[test]
    fn env_applies_to_run_and_background() {
        let temp = tempfile::tempdir().unwrap();
        let root = guard_root(temp.path());

        let script = if cfg!(windows) {
            indoc! {
                r#"
                ENV FOO=bar
                RUN echo %FOO% > run.txt
                RUN_BG echo %FOO% > bg.txt
                "#
            }
        } else {
            indoc! {
                r#"
                ENV FOO=bar
                RUN sh -c 'printf %s "$FOO" > run.txt'
                RUN_BG sh -c 'printf %s "$FOO" > bg.txt'
                "#
            }
        };

        let steps = parse_script(script).unwrap();
        run_steps(&root, &steps).unwrap();

        assert_eq!(read_trimmed(&root.join("run.txt").unwrap()), "bar");
        assert_eq!(read_trimmed(&root.join("bg.txt").unwrap()), "bar");
    }

    #[test]
    fn workspace_switches_between_snapshot_and_local() {
        let snapshot = tempfile::tempdir().unwrap();
        let local = tempfile::tempdir().unwrap();
        let snapshot_root = guard_root(snapshot.path());
        let local_root = guard_root(local.path());

        let script = indoc! {
            r#"
            WRITE snap.txt snap
            WORKSPACE LOCAL
            WRITE local.txt local
            WORKSPACE SNAPSHOT
            WRITE snap2.txt again
            "#
        };

        let steps = parse_script(script).unwrap();
        run_steps_with_context(&snapshot_root, &local_root, &steps).unwrap();

        assert!(snapshot_root.as_path().join("snap.txt").exists());
        assert!(snapshot_root.as_path().join("snap2.txt").exists());
        assert!(local_root.as_path().join("local.txt").exists());
    }

    #[test]
    fn workspace_root_changes_where_slash_points() {
        let snapshot = tempfile::tempdir().unwrap();
        let local = tempfile::tempdir().unwrap();
        let snapshot_root = guard_root(snapshot.path());
        let local_root = guard_root(local.path());
        let local_client = local_root.join("client").unwrap();
        create_dirs(&local_client);

        let script = indoc! {
            r#"
            WORKSPACE LOCAL
            WORKDIR /
            WRITE localroot.txt one
            WORKDIR client
            WRITE client.txt two
            WORKSPACE SNAPSHOT
            WORKDIR /
            WRITE snaproot.txt three
            "#
        };

        let steps = parse_script(script).unwrap();
        run_steps_with_context(&snapshot_root, &local_root, &steps).unwrap();

        assert!(local_root.as_path().join("localroot.txt").exists());
        assert!(local_client.as_path().join("client.txt").exists());
        assert!(snapshot_root.as_path().join("snaproot.txt").exists());
    }
}
