use oxdock_cli::{Step, StepKind, run_script};
use oxdock_fs::{GuardedPath, PathResolver};

#[cfg_attr(miri, ignore = "spawns subprocesses; process spawning not supported under Miri")]
#[test]
fn script_runs_copy_and_symlink() {
    let temp = GuardedPath::tempdir().unwrap();
    let root = temp.as_guarded_path().clone();

    // Prepare minimal workspace structure using the PathResolver to avoid
    // calling `std::fs` directly from non-fs crates.
    let resolver = PathResolver::new(root.as_path(), root.as_path()).unwrap();
    resolver
        .create_dir_all_abs(&root.join("client/dist").unwrap())
        .unwrap();
    resolver
        .create_dir_all_abs(&root.join("server").unwrap())
        .unwrap();
    // Seed dist with a file
    resolver
        .write_file(&root.join("client/dist/test.txt").unwrap(), b"hello\n")
        .unwrap();

    let steps = vec![
        Step {
            guards: Vec::new(),
            kind: StepKind::Workdir("client".into()),
        },
        Step {
            guards: Vec::new(),
            kind: StepKind::Run("echo ok".into()),
        },
        Step {
            guards: Vec::new(),
            kind: StepKind::Workdir("/".into()),
        },
        Step {
            guards: Vec::new(),
            kind: StepKind::Copy {
                from: "./client/dist".into(),
                to: "./client/dist-copy".into(),
            },
        },
        Step {
            guards: Vec::new(),
            kind: StepKind::Symlink {
                from: "./client/dist".into(),
                to: "./server/dist".into(),
            },
        },
        Step {
            guards: Vec::new(),
            kind: StepKind::Run("echo ok".into()),
        },
    ];

    run_script(&root, &steps).unwrap();

    // Copy should exist and contain the file
    let copied = root.join("client/dist-copy/test.txt").unwrap();
    assert!(copied.as_path().exists());
    let contents = resolver.read_to_string(&copied).unwrap();
    assert!(contents.contains("hello"));

    // Symlink should resolve to dist (on Unix); on non-Unix we copied.
    #[cfg(unix)]
    {
        let linked = root.join("server/dist/test.txt").unwrap();
        assert!(linked.as_path().exists());
    }
    #[cfg(not(unix))]
    {
        // On non-Unix, symlink_dir may not be available; ensure copy fallback works.
        let linked_copy = root.join("server/dist/test.txt").unwrap();
        assert!(linked_copy.as_path().exists());
    }
}
