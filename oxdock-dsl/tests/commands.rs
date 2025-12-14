use oxdock_dsl::{run_steps, run_steps_with_context, Step, StepKind, WorkspaceTarget};
use std::fs;
use tempfile::tempdir;

fn read_trimmed(path: &std::path::Path) -> String {
    fs::read_to_string(path)
        .unwrap_or_default()
        .trim()
        .to_string()
}

#[test]
fn commands_behave_cross_platform() {
    let snapshot = tempdir().unwrap();
    let local = tempdir().unwrap();

    // Build context (local workspace) files for COPY and SYMLINK targets.
    let build_root = local.path();
    fs::write(build_root.join("source.txt"), "from build").unwrap();
    let target_dir = build_root.join("target_dir");
    fs::create_dir_all(&target_dir).unwrap();
    fs::write(target_dir.join("inner.txt"), "symlink target").unwrap();

    let run_cmd = if cfg!(windows) {
        "echo %FOO%> run.txt"
    } else {
        "printf %s \"$FOO\" > run.txt"
    };

    // Background command should stay alive long enough for the foreground steps to complete.
    let bg_cmd = if cfg!(windows) {
        "ping -n 3 127.0.0.1 > NUL & echo %FOO%> bg.txt"
    } else {
        "sleep 0.2; printf %s \"$FOO\" > bg.txt"
    };

    let link_target = snapshot
        .path()
        .join("client/dist")
        .to_string_lossy()
        .to_string();

    let steps = vec![
        Step {
            guards: Vec::new(),
            kind: StepKind::Workdir("/".into()),
        },
        Step {
            guards: Vec::new(),
            kind: StepKind::Mkdir("client".into()),
        },
        Step {
            guards: Vec::new(),
            kind: StepKind::Mkdir("client/dist".into()),
        },
        Step {
            guards: Vec::new(),
            kind: StepKind::Write {
                path: "client/dist/hello.txt".into(),
                contents: "hi".into(),
            },
        },
        Step {
            guards: Vec::new(),
            kind: StepKind::Env {
                key: "FOO".into(),
                value: "bar".into(),
            },
        },
        Step {
            guards: Vec::new(),
            kind: StepKind::Run(run_cmd.into()),
        },
        Step {
            guards: Vec::new(),
            kind: StepKind::RunBg(bg_cmd.into()),
        },
        Step {
            guards: Vec::new(),
            kind: StepKind::Copy {
                from: "source.txt".into(),
                to: "client/dist/from_build.txt".into(),
            },
        },
        Step {
            guards: Vec::new(),
            kind: StepKind::Symlink {
                link: "client/dist-link".into(),
                target: link_target.clone(),
            },
        },
        Step {
            guards: Vec::new(),
            kind: StepKind::Ls(Some("client".into())),
        },
        Step {
            guards: Vec::new(),
            kind: StepKind::Workdir("client/dist".into()),
        },
        Step {
            guards: Vec::new(),
            kind: StepKind::Echo("echo from workdir".into()),
        },
        Step {
            guards: Vec::new(),
            kind: StepKind::Write {
                path: "nested.txt".into(),
                contents: "nested".into(),
            },
        },
        Step {
            guards: Vec::new(),
            kind: StepKind::Workspace(WorkspaceTarget::Local),
        },
        Step {
            guards: Vec::new(),
            kind: StepKind::Workdir("/".into()),
        },
        Step {
            guards: Vec::new(),
            kind: StepKind::Write {
                path: "local_note.txt".into(),
                contents: "local".into(),
            },
        },
        Step {
            guards: Vec::new(),
            kind: StepKind::Workspace(WorkspaceTarget::Snapshot),
        },
        Step {
            guards: Vec::new(),
            kind: StepKind::Workdir("/".into()),
        },
        Step {
            guards: Vec::new(),
            kind: StepKind::Write {
                path: "snap_note.txt".into(),
                contents: "snap".into(),
            },
        },
    ];

    run_steps_with_context(snapshot.path(), local.path(), &steps).unwrap();

    // RUN picks up ENV
    assert_eq!(read_trimmed(&snapshot.path().join("run.txt")), "bar");
    // RUN_BG picks up ENV
    assert_eq!(read_trimmed(&snapshot.path().join("bg.txt")), "bar");

    // WRITE + MKDIR
    assert_eq!(
        read_trimmed(&snapshot.path().join("client/dist/hello.txt")),
        "hi"
    );
    assert_eq!(
        read_trimmed(&snapshot.path().join("client/dist/nested.txt")),
        "nested"
    );

    // COPY from build context into snapshot workspace
    assert_eq!(
        read_trimmed(&snapshot.path().join("client/dist/from_build.txt")),
        "from build"
    );

    // SYMLINK resolves to target dir and exposes contents
    let linked_file = snapshot.path().join("client/dist-link/hello.txt");
    assert!(linked_file.exists(), "symlink should point at target contents");
    assert_eq!(read_trimmed(&linked_file), "hi");

    // WORKSPACE switches between snapshot and local roots
    assert_eq!(read_trimmed(&local.path().join("local_note.txt")), "local");
    assert_eq!(read_trimmed(&snapshot.path().join("snap_note.txt")), "snap");
}

#[test]
fn exit_stops_pipeline_and_reports_code() {
    let root = tempdir().unwrap();
    let steps = vec![
        Step {
            guards: Vec::new(),
            kind: StepKind::Write {
                path: "before.txt".into(),
                contents: "ok".into(),
            },
        },
        Step {
            guards: Vec::new(),
            kind: StepKind::Exit(9),
        },
        Step {
            guards: Vec::new(),
            kind: StepKind::Write {
                path: "after.txt".into(),
                contents: "nope".into(),
            },
        },
    ];

    let err = run_steps(root.path(), &steps).unwrap_err();
    assert!(
        err.to_string().contains("EXIT requested with code 9"),
        "error message should surface EXIT code"
    );

    assert!(root.path().join("before.txt").exists());
    assert!(!root.path().join("after.txt").exists());
}
