use oxdock_fs::{
    GuardPolicy, GuardedPath, PolicyPath, discover_workspace_root, embed_path, to_forward_slashes,
    WorkspaceFs,
};
#[allow(clippy::disallowed_types)]
use oxdock_fs::UnguardedPath;
use std::env;

struct EnvGuard {
    key: &'static str,
    value: Option<String>,
}

impl EnvGuard {
    fn set(key: &'static str, value: &str) -> Self {
        let prev = env::var(key).ok();
        unsafe {
            env::set_var(key, value);
        }
        Self { key, value: prev }
    }
}

impl Drop for EnvGuard {
    fn drop(&mut self) {
        match &self.value {
            Some(value) => unsafe {
                env::set_var(self.key, value);
            },
            None => unsafe {
                env::remove_var(self.key);
            },
        }
    }
}

#[cfg_attr(
    miri,
    ignore = "GuardedPath::tempdir relies on OS tempdirs; blocked under Miri isolation"
)]
#[test]
fn guarded_path_join_parent_and_display() {
    let tempdir = GuardedPath::tempdir().expect("tempdir");
    let root = tempdir.as_guarded_path().clone();
    let nested = root.join("a/b").expect("join");
    let parent = nested.parent().expect("parent");
    assert_eq!(parent, root.join("a").expect("parent join"));
    let display = nested.display();
    assert!(!display.is_empty());
}

#[cfg_attr(
    miri,
    ignore = "GuardedPath::tempdir relies on OS tempdirs; blocked under Miri isolation"
)]
#[test]
fn policy_path_accessors_and_policy() {
    let tempdir = GuardedPath::tempdir().expect("tempdir");
    let guarded = tempdir.as_guarded_path().clone();
    #[allow(clippy::disallowed_types)]
    let unguarded = UnguardedPath::new(guarded.as_path());
    let guarded_policy = PolicyPath::from(guarded.clone());
    let unguarded_policy = PolicyPath::from(unguarded.clone());
    assert_eq!(guarded_policy.policy(), GuardPolicy::Guarded);
    assert_eq!(unguarded_policy.policy(), GuardPolicy::Unguarded);
    assert_eq!(guarded_policy.as_guarded(), Some(&guarded));
    assert_eq!(unguarded_policy.as_unguarded(), Some(&unguarded));
}

#[cfg_attr(
    miri,
    ignore = "GuardedPath::tempdir relies on OS tempdirs; blocked under Miri isolation"
)]
#[test]
fn forward_slash_helpers_normalize_paths() {
    let tempdir = GuardedPath::tempdir().expect("tempdir");
    let root = tempdir.as_guarded_path().clone();
    let child = root.join("a/b").expect("join");
    let embedded = embed_path(&child);
    assert!(!embedded.contains('\\'));
    assert_eq!(to_forward_slashes("a\\b"), "a/b");
}

#[cfg_attr(
    miri,
    ignore = "GuardedPath::tempdir relies on OS tempdirs; blocked under Miri isolation"
)]
#[test]
fn discover_workspace_root_prefers_env() {
    let tempdir = GuardedPath::tempdir().expect("tempdir");
    let root = tempdir.as_guarded_path().clone();
    let _guard = EnvGuard::set("OXDOCK_WORKSPACE_ROOT", root.display().as_str());
    let discovered = discover_workspace_root().expect("discover");
    assert_eq!(discovered.as_path(), root.as_path());
}

#[cfg(feature = "mock-fs")]
#[test]
fn mock_fs_writes_and_reads_files() {
    use oxdock_fs::{EntryKind, MockFs, WorkspaceFs};

    let fs = MockFs::new();
    let root = fs.root().clone();
    let file = root.join("dir/file.txt").expect("file path");
    fs.ensure_parent_dir(&file).expect("ensure parent");
    fs.write_file(&file, b"hello").expect("write");
    let contents = fs.read_to_string(&file).expect("read");
    assert_eq!(contents, "hello");
    let dir = root.join("dir").expect("dir path");
    assert_eq!(fs.entry_kind(&dir).expect("dir kind"), EntryKind::Dir);
    assert_eq!(
        fs.entry_kind(&file).expect("file kind"),
        EntryKind::File
    );
}

#[cfg(feature = "mock-fs")]
#[test]
fn mock_fs_resolves_relative_paths() {
    use oxdock_fs::MockFs;

    let fs = MockFs::new();
    let root = fs.root().clone();
    let nested = fs.resolve_workdir(&root, "a/b").expect("resolve");
    let expected = root.join("a/b").expect("expected");
    assert_eq!(nested, expected);
    let parent = fs.resolve_workdir(&nested, "..").expect("parent");
    assert_eq!(parent, root.join("a").expect("parent"));
}
