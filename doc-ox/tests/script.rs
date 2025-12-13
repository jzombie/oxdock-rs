use std::fs::{self, File};
use std::io::Write;

use doc_ox::{Step, StepKind, run_script};

#[test]
fn script_runs_copy_and_symlink() {
    let temp = tempfile::tempdir().unwrap();
    let root = temp.path();

    // Prepare minimal workspace structure
    fs::create_dir_all(root.join("client/dist")).unwrap();
    fs::create_dir_all(root.join("server")).unwrap();

    // Seed dist with a file
    let mut f = File::create(root.join("client/dist/test.txt")).unwrap();
    writeln!(f, "hello").unwrap();

    let steps = vec![
        Step {
            guards: Vec::new(),
            kind: StepKind::Workdir("client".into()),
        },
        Step {
            guards: Vec::new(),
            kind: StepKind::Run("true".into()),
        },
        Step {
            guards: Vec::new(),
            kind: StepKind::Workdir("/".into()),
        },
        Step {
            guards: Vec::new(),
            kind: StepKind::Copy {
                from: "client/dist".into(),
                to: "client/dist-copy".into(),
            },
        },
        Step {
            guards: Vec::new(),
            kind: StepKind::Symlink {
                link: "server/dist".into(),
                target: "client/dist".into(),
            },
        },
        Step {
            guards: Vec::new(),
            kind: StepKind::Run("true".into()),
        },
    ];

    run_script(root, &steps).unwrap();

    // Copy should exist and contain the file
    let copied = root.join("client/dist-copy/test.txt");
    assert!(copied.exists());
    let contents = fs::read_to_string(copied).unwrap();
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
