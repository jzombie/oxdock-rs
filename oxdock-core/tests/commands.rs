use oxdock_core::{Step, StepKind, WorkspaceTarget, run_steps, run_steps_with_context};
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
                from: "./source.txt".into(),
                to: "./client/dist/from_build.txt".into(),
            },
        },
        Step {
            guards: Vec::new(),
            kind: StepKind::Symlink {
                from: "./target_dir".into(),
                to: "./client/dist-link".into(),
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

    // SYMLINK resolves to target dir (with ./ prefix) and exposes contents
    let linked_file = snapshot.path().join("client/dist-link/inner.txt");
    assert!(
        linked_file.exists(),
        "symlink should point at target contents"
    );
    assert_eq!(read_trimmed(&linked_file), "symlink target");

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

#[test]
fn accepts_semicolon_separated_commands() {
    let root = tempdir().unwrap();
    let script = "WRITE one.txt 1; WRITE two.txt 2";
    let steps = oxdock_core::parse_script(script).unwrap();
    run_steps(root.path(), &steps).unwrap();
    assert_eq!(read_trimmed(&root.path().join("one.txt")), "1");
    assert_eq!(read_trimmed(&root.path().join("two.txt")), "2");
}

#[test]
fn write_cmd_captures_output() {
    let root = tempdir().unwrap();
    let cmd = if cfg!(windows) { "echo hello" } else { "printf %s \"hello\"" };
    let steps = vec![
        Step {
            guards: Vec::new(),
            kind: StepKind::Capture {
                path: "out.txt".into(),
                cmd: cmd.into(),
            },
        },
    ];
    run_steps(root.path(), &steps).unwrap();
    assert_eq!(read_trimmed(&root.path().join("out.txt")), "hello");
}

#[test]
fn copy_git_via_script_simple() {
    let snapshot = tempdir().unwrap();

    // Create a tiny git repo inside the snapshot so build_context is under root
    let repo = snapshot.path().join("repo");
    std::fs::create_dir_all(&repo).unwrap();
    std::fs::write(repo.join("hello.txt"), "git hello").unwrap();
    std::fs::create_dir_all(repo.join("assets")).unwrap();
    std::fs::write(repo.join("assets").join("a.txt"), "a").unwrap();
    std::fs::write(repo.join("assets").join("b.txt"), "b").unwrap();

    // init and commit
    std::process::Command::new("git")
        .arg("-C")
        .arg(&repo)
        .arg("init")
        .arg("-q")
        .status()
        .expect("git init failed");
    std::process::Command::new("git")
        .arg("-C")
        .arg(&repo)
        .arg("add")
        .arg(".")
        .status()
        .expect("git add failed");
    // Commit using `-c` so we don't write any repo config
    std::process::Command::new("git")
        .arg("-C")
        .arg(&repo)
        .arg("-c")
        .arg("user.email=test@example.com")
        .arg("-c")
        .arg("user.name=Test User")
        .arg("commit")
        .arg("-m")
        .arg("initial")
        .status()
        .expect("git commit failed");

    let rev_out = std::process::Command::new("git")
        .arg("-C")
        .arg(&repo)
        .arg("rev-parse")
        .arg("HEAD")
        .output()
        .expect("git rev-parse failed");
    let rev = String::from_utf8_lossy(&rev_out.stdout).trim().to_string();

    let script = format!("COPY_GIT {} hello.txt out_hello.txt", rev);

    let steps = oxdock_core::parse_script(&script).unwrap();
    // build_context is `repo` which is under `snapshot` root
    run_steps_with_context(snapshot.path(), &repo, &steps).unwrap();

    assert_eq!(
        std::fs::read_to_string(snapshot.path().join("out_hello.txt")).unwrap(),
        "git hello"
    );
}

#[test]
fn copy_git_directory_via_script() {
    let snapshot = tempdir().unwrap();

    // Create a tiny git repo inside the snapshot so build_context is under root
    let repo = snapshot.path().join("repo_dir");
    std::fs::create_dir_all(&repo).unwrap();
    std::fs::create_dir_all(repo.join("assets_dir")).unwrap();
    std::fs::write(repo.join("assets_dir").join("x.txt"), "x").unwrap();
    std::fs::write(repo.join("assets_dir").join("y.txt"), "y").unwrap();

    // init, add, commit (use -c to avoid writing config)
    std::process::Command::new("git")
        .arg("-C")
        .arg(&repo)
        .arg("init")
        .arg("-q")
        .status()
        .expect("git init failed");
    std::process::Command::new("git")
        .arg("-C")
        .arg(&repo)
        .arg("add")
        .arg(".")
        .status()
        .expect("git add failed");
    std::process::Command::new("git")
        .arg("-C")
        .arg(&repo)
        .arg("-c")
        .arg("user.email=test@example.com")
        .arg("-c")
        .arg("user.name=Test User")
        .arg("commit")
        .arg("-m")
        .arg("initial")
        .status()
        .expect("git commit failed");

    let rev_out = std::process::Command::new("git")
        .arg("-C")
        .arg(&repo)
        .arg("rev-parse")
        .arg("HEAD")
        .output()
        .expect("git rev-parse failed");
    let rev = String::from_utf8_lossy(&rev_out.stdout).trim().to_string();

    let script = format!("COPY_GIT {} assets_dir out_assets_dir", rev);
    let steps = oxdock_core::parse_script(&script).unwrap();
    run_steps_with_context(snapshot.path(), &repo, &steps).unwrap();

    assert_eq!(
        std::fs::read_to_string(snapshot.path().join("out_assets_dir").join("x.txt")).unwrap(),
        "x"
    );
    assert_eq!(
        std::fs::read_to_string(snapshot.path().join("out_assets_dir").join("y.txt")).unwrap(),
        "y"
    );
}

#[test]
fn workdir_cannot_escape_root() {
    let root = tempdir().unwrap();
    // Attempt to switch to parent of root which should be disallowed
    let steps = vec![Step {
        guards: Vec::new(),
        kind: StepKind::Workdir("../".into()),
    }];

    let err = run_steps(root.path(), &steps).unwrap_err();
    assert!(
        err.to_string().contains("WORKDIR") && err.to_string().contains("escapes"),
        "expected WORKDIR escape error, got {}",
        err
    );
}

#[test]
fn write_cannot_escape_root() {
    let root = tempdir().unwrap();
    let steps = vec![Step {
        guards: Vec::new(),
        kind: StepKind::Write {
            path: "../escape.txt".into(),
            contents: "nope".into(),
        },
    }];

    let err = run_steps(root.path(), &steps).unwrap_err();
    assert!(
        err.to_string().contains("WRITE") && err.to_string().contains("escapes"),
        "expected WRITE escape error, got {}",
        err
    );
}

#[test]
fn read_cannot_escape_root() {
    let root = tempdir().unwrap();
    let parent = root.path().parent().expect("tempdir should have a parent");
    let secret = parent.join(format!(
        "{}-secret.txt",
        root.path()
            .file_name()
            .and_then(|n| n.to_str())
            .unwrap_or("escape")
    ));
    fs::write(&secret, "nope").unwrap();

    let steps = vec![Step {
        guards: Vec::new(),
        kind: StepKind::Cat("../secret.txt".into()),
    }];

    let err = run_steps(root.path(), &steps).unwrap_err();
    assert!(
        err.to_string().contains("CAT") && err.to_string().contains("escapes"),
        "expected CAT escape error, got {}",
        err
    );

    let _ = fs::remove_file(&secret);
}

#[test]
fn read_symlink_escape_is_blocked() {
    let root = tempdir().unwrap();
    let parent = root.path().parent().expect("tempdir should have a parent");
    let secret = parent.join(format!(
        "{}-symlink-secret.txt",
        root.path()
            .file_name()
            .and_then(|n| n.to_str())
            .unwrap_or("escape")
    ));
    fs::write(&secret, "top secret").unwrap();

    // Inside root, create a link that points to the outside secret.
    let link_path = root.path().join("leak.txt");
    #[cfg(unix)]
    std::os::unix::fs::symlink(&secret, &link_path).unwrap();
    #[cfg(windows)]
    std::os::windows::fs::symlink_file(&secret, &link_path).unwrap();

    let steps = vec![Step {
        guards: Vec::new(),
        kind: StepKind::Cat("leak.txt".into()),
    }];

    let err = run_steps(root.path(), &steps).unwrap_err();
    assert!(
        err.to_string().contains("CAT") && err.to_string().contains("escapes"),
        "expected CAT symlink escape error, got {}",
        err
    );

    let _ = fs::remove_file(&secret);
}

#[test]
fn write_missing_path_cannot_escape_root() {
    let root = tempdir().unwrap();
    fs::create_dir_all(root.path().join("a/b")).unwrap();

    let steps = vec![Step {
        guards: Vec::new(),
        kind: StepKind::Write {
            // Ancestor exists inside root, but remaining components attempt to climb out.
            path: "a/b/../../../../outside.txt".into(),
            contents: "nope".into(),
        },
    }];

    let err = run_steps(root.path(), &steps).unwrap_err();
    assert!(
        err.to_string().contains("WRITE") && err.to_string().contains("escapes"),
        "expected WRITE escape error for missing path, got {}",
        err
    );
}

#[test]
fn workdir_creates_missing_dirs_within_root() {
    let root = tempdir().unwrap();
    let steps = vec![Step {
        guards: Vec::new(),
        kind: StepKind::Workdir("a/b/c".into()),
    }];

    run_steps(root.path(), &steps).unwrap();

    assert!(root.path().join("a/b/c").exists());
}

#[test]
fn cat_reads_file_contents_without_error() {
    let root = tempdir().unwrap();
    fs::write(root.path().join("file.txt"), "hello cat").unwrap();
    let steps = vec![Step {
        guards: Vec::new(),
        kind: StepKind::Cat("file.txt".into()),
    }];

    // This should succeed and emit contents to stdout; we only verify it does not error.
    run_steps(root.path(), &steps).unwrap();
}
