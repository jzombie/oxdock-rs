use oxdock_cli::{Step, StepKind, run_script};
use oxdock_fs::PathResolver;

#[test]
fn script_runs_copy_and_symlink() {
    let temp = tempfile::tempdir().unwrap();
    let root = temp.path();

    // Prepare minimal workspace structure using the PathResolver to avoid
    // calling `std::fs` directly from non-fs crates.
    let resolver = PathResolver::new(root, root);
    resolver
        .create_dir_all_abs(&root.join("client/dist"))
        .unwrap();
    resolver.create_dir_all_abs(&root.join("server")).unwrap();
    // Seed dist with a file
    resolver
        .write_file(&root.join("client/dist/test.txt"), b"hello\n")
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

    run_script(root, &steps).unwrap();

    // Copy should exist and contain the file
    let copied = root.join("client/dist-copy/test.txt");
    assert!(copied.exists());
    let contents = resolver.read_to_string(&copied).unwrap();
    assert!(contents.contains("hello"));

    // Symlink should resolve to dist (on Unix); on non-Unix we copied.
    #[cfg(unix)]
    {
        let linked = root.join("server/dist/test.txt");
        assert!(linked.exists());
    }
    #[cfg(not(unix))]
    {
        // On non-Unix, symlink_dir may not be available; ensure copy fallback works.
        let linked_copy = root.join("server/dist/test.txt");
        assert!(linked_copy.exists());
    }
}
