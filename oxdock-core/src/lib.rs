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
    #[cfg(unix)]
    use std::time::Instant;

    #[test]
    fn run_sets_cargo_target_dir_to_fs_root() {
        let temp = tempfile::tempdir().unwrap();
        let root = temp.path();

        let cmd = if cfg!(windows) {
            "echo %CARGO_TARGET_DIR% > seen.txt"
        } else {
            "printf %s \"$CARGO_TARGET_DIR\" > seen.txt"
        };

        let steps = vec![Step {
            guards: Vec::new(),
            kind: StepKind::Run(cmd.to_string()),
        }];

        run_steps(root, &steps).unwrap();

        let seen = std::fs::read_to_string(root.join("seen.txt")).unwrap();
        let expected = root.join(".cargo-target");
        assert_eq!(
            seen.trim(),
            expected.to_string_lossy(),
            "CARGO_TARGET_DIR should be scoped"
        );
    }

    #[test]
    fn guard_skips_when_env_missing() {
        let temp = tempfile::tempdir().unwrap();
        let root = temp.path();

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

        run_steps(root, &steps).unwrap();

        assert!(
            !root.join("skipped.txt").exists(),
            "guarded WRITE should be skipped"
        );
        assert!(root.join("kept.txt").exists(), "unguarded WRITE should run");
    }

    #[test]
    fn guard_sees_env_set_by_env_step() {
        let temp = tempfile::tempdir().unwrap();
        let root = temp.path();

        let script = indoc!(
            r#"
            ENV FOO=1
            [env:FOO] WRITE hit.txt yes
            WRITE always.txt ok
            "#
        );
        let steps = parse_script(script).unwrap();

        run_steps(root, &steps).unwrap();

        assert!(
            root.join("hit.txt").exists(),
            "guarded WRITE should run after ENV sets variable"
        );
        assert!(
            root.join("always.txt").exists(),
            "unguarded WRITE should run"
        );
    }

    #[test]
    fn echo_runs_and_allows_subsequent_steps() {
        let temp = tempfile::tempdir().unwrap();
        let root = temp.path();

        let script = indoc!(
            r#"
            ECHO Hello, world
            WRITE always.txt ok
            "#
        );
        let steps = parse_script(script).unwrap();

        run_steps(root, &steps).unwrap();

        assert!(
            root.join("always.txt").exists(),
            "WRITE after ECHO should run"
        );
    }

    #[test]
    fn guard_on_previous_line_applies_to_next_command() {
        let temp = tempfile::tempdir().unwrap();
        let root = temp.path();

        let script = indoc!(
            r#"
            ENV FOO=1
            [env:FOO]
            WRITE hit.txt yes
            WRITE always.txt ok
            "#
        );
        let steps = parse_script(script).unwrap();

        run_steps(root, &steps).unwrap();

        assert!(
            root.join("hit.txt").exists(),
            "guarded WRITE on next line should run"
        );
        assert!(
            root.join("always.txt").exists(),
            "unguarded WRITE should run"
        );
    }

    #[test]
    fn guard_respects_platform_negation() {
        let temp = tempfile::tempdir().unwrap();
        let root = temp.path();

        let script = indoc!(
            r#"
            [!unix] WRITE platform.txt hi
            WRITE always.txt ok
            "#
        );
        let steps = parse_script(script).unwrap();

        run_steps(root, &steps).unwrap();

        let expect_skipped = cfg!(unix);
        assert_eq!(
            root.join("platform.txt").exists(),
            !expect_skipped,
            "platform guard should skip on unix and run elsewhere"
        );
        assert!(
            root.join("always.txt").exists(),
            "unguarded WRITE should run"
        );
    }

    #[test]
    fn guard_matches_profile_env() {
        // Cargo sets PROFILE during builds/tests; verify guards see it.
        let temp = tempfile::tempdir().unwrap();
        let root = temp.path();

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
        run_steps(root, &steps).unwrap();

        assert!(
            root.join("hit.txt").exists(),
            "PROFILE-matching guard should run"
        );
        assert!(
            !root.join("miss.txt").exists(),
            "PROFILE inequality guard should skip for current profile"
        );
    }

    #[test]
    fn multiple_guards_all_must_pass() {
        let temp = tempfile::tempdir().unwrap();
        let root = temp.path();

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
        run_steps(root, &steps).unwrap();

        assert!(
            root.join("hit.txt").exists(),
            "guarded step should run when all guards pass"
        );
        assert!(
            root.join("always.txt").exists(),
            "unguarded step should run"
        );
    }

    #[test]
    fn multiple_guards_skip_when_one_fails() {
        let temp = tempfile::tempdir().unwrap();
        let root = temp.path();

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
        run_steps(root, &steps).unwrap();

        assert!(
            !root.join("miss.txt").exists(),
            "guarded step should skip when any guard fails"
        );
        assert!(
            root.join("always.txt").exists(),
            "unguarded step should run"
        );
    }

    #[cfg(unix)]
    #[test]
    fn run_bg_exits_success_and_stops_pipeline() {
        let temp = tempfile::tempdir().unwrap();
        let root = temp.path();

        // Background succeeds quickly; pipeline should complete without error.
        let script = "RUN_BG sh -c 'sleep 0.05'";
        let steps = parse_script(script).unwrap();
        let res = run_steps(root, &steps);
        assert!(res.is_ok(), "RUN_BG success should allow clean exit");
    }

    #[cfg(unix)]
    #[test]
    fn run_bg_failure_bubbles_status() {
        let temp = tempfile::tempdir().unwrap();
        let root = temp.path();

        let script = "RUN_BG sh -c 'sleep 0.05; exit 7'";
        let steps = parse_script(script).unwrap();
        let err = run_steps(root, &steps).unwrap_err();
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
        let root = temp.path();

        let script = indoc! {
            r#"
            RUN_BG sh -c 'sleep 0.2; echo one > one.txt'
            RUN_BG sh -c 'sleep 0.5; echo two > two.txt'
            WRITE done.txt ok
            "#
        };

        let steps = parse_script(script).unwrap();
        let start = Instant::now();
        let res = run_steps(root, &steps);
        let elapsed = start.elapsed();

        assert!(res.is_ok(), "RUN_BG success should allow clean exit");
        assert!(
            root.join("done.txt").exists(),
            "foreground step should run after spawning backgrounds"
        );
        assert!(
            root.join("one.txt").exists(),
            "first background should finish and emit output"
        );
        assert!(
            !root.join("two.txt").exists(),
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
        let root = temp.path();

        let script = indoc! {
            r#"
            RUN_BG sh -c 'sleep 1; echo late > late.txt'
            EXIT 5
            "#
        };

        let steps = parse_script(script).unwrap();
        let err = run_steps(root, &steps).unwrap_err();
        let msg = err.to_string();
        assert!(msg.contains("EXIT requested with code 5"));
        assert!(
            !root.join("late.txt").exists(),
            "background process should be killed when EXIT is hit"
        );
    }

    #[test]
    fn env_applies_to_run_and_background() {
        let temp = tempfile::tempdir().unwrap();
        let root = temp.path();

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
        run_steps(root, &steps).unwrap();

        assert_eq!(
            std::fs::read_to_string(root.join("run.txt"))
                .unwrap()
                .trim(),
            "bar"
        );
        assert_eq!(
            std::fs::read_to_string(root.join("bg.txt")).unwrap().trim(),
            "bar"
        );
    }

    #[test]
    fn workspace_switches_between_snapshot_and_local() {
        let snapshot = tempfile::tempdir().unwrap();
        let local = tempfile::tempdir().unwrap();

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
        run_steps_with_context(snapshot.path(), local.path(), &steps).unwrap();

        assert!(snapshot.path().join("snap.txt").exists());
        assert!(snapshot.path().join("snap2.txt").exists());
        assert!(local.path().join("local.txt").exists());
    }

    #[test]
    fn workspace_root_changes_where_slash_points() {
        let snapshot = tempfile::tempdir().unwrap();
        let local = tempfile::tempdir().unwrap();
        let local_client = local.path().join("client");
        std::fs::create_dir_all(&local_client).unwrap();

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
        run_steps_with_context(snapshot.path(), local.path(), &steps).unwrap();

        assert!(local.path().join("localroot.txt").exists());
        assert!(local_client.join("client.txt").exists());
        assert!(snapshot.path().join("snaproot.txt").exists());
    }
}
