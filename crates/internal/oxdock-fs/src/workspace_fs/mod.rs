// Workspace-scoped path resolver with guarded file operations.
// Methods are split across submodules by concern (access checks, IO, copy, git, resolve helpers).

use anyhow::{Context, Result};
#[cfg(miri)]
use std::cell::RefCell;
#[cfg(miri)]
use std::collections::{BTreeMap, HashMap, HashSet};
#[cfg(miri)]
use std::rc::Rc;

#[cfg(not(miri))]
pub type DirEntry = std::fs::DirEntry;
#[cfg(not(miri))]
pub type FileType = std::fs::FileType;

#[cfg(miri)]
mod synthetic_entry {
    use super::EntryKind;
    use std::ffi::OsString;
    use std::io;
    use std::path::PathBuf;

    #[derive(Clone, Copy)]
    pub struct FileType {
        kind: EntryKind,
    }

    impl FileType {
        pub fn is_dir(&self) -> bool {
            matches!(self.kind, EntryKind::Dir)
        }

        pub fn is_file(&self) -> bool {
            matches!(self.kind, EntryKind::File)
        }

        pub fn is_symlink(&self) -> bool {
            false
        }
    }

    #[derive(Clone)]
    pub struct DirEntry {
        path: PathBuf,
        file_name: OsString,
        file_type: FileType,
    }

    impl DirEntry {
        pub(crate) fn new(path: PathBuf, kind: EntryKind) -> Self {
            let file_name = path
                .file_name()
                .map(|n| n.to_os_string())
                .unwrap_or_else(|| OsString::new());
            Self {
                path,
                file_name,
                file_type: FileType { kind },
            }
        }

        pub fn path(&self) -> PathBuf {
            self.path.clone()
        }

        pub fn file_name(&self) -> OsString {
            self.file_name.clone()
        }

        pub fn file_type(&self) -> io::Result<FileType> {
            Ok(self.file_type)
        }
    }

    impl std::fmt::Debug for DirEntry {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            f.debug_struct("DirEntry")
                .field("path", &self.path)
                .finish()
        }
    }

    impl std::fmt::Debug for FileType {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            f.debug_struct("FileType")
                .field("kind", &self.kind)
                .finish()
        }
    }
}

#[cfg(miri)]
pub use synthetic_entry::{DirEntry, FileType};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum EntryKind {
    File,
    Dir,
}

pub mod path;
pub use path::{GuardedPath, GuardedTempDir};

#[allow(clippy::disallowed_types)]
pub use path::UnguardedPath;

#[allow(clippy::disallowed_types)]
use std::path::Path;

#[derive(Clone, Copy, Debug)]
pub(crate) enum AccessMode {
    Read,
    Write,
    Passthru,
}

impl AccessMode {
    fn name(&self) -> &'static str {
        match self {
            AccessMode::Read => "READ",
            AccessMode::Write => "WRITE",
            AccessMode::Passthru => "PASSTHRU",
        }
    }
}

/// Resolves and validates filesystem paths within a confined workspace and build context.
pub struct PathResolver {
    root: GuardedPath,
    build_context: GuardedPath,
    #[cfg(miri)]
    root_state: Rc<RefCell<SyntheticRootState>>,
    #[cfg(miri)]
    build_state: Rc<RefCell<SyntheticRootState>>,
}

impl PathResolver {
    /// Build a resolver rooted at `CARGO_MANIFEST_DIR`, using that same
    /// directory as the build context. This centralizes env lookup and path
    /// creation so callers avoid ad-hoc path construction.
    #[allow(clippy::disallowed_types, clippy::disallowed_methods)]
    pub fn from_manifest_env() -> Result<Self> {
        let manifest_dir =
            std::env::var("CARGO_MANIFEST_DIR").context("CARGO_MANIFEST_DIR missing")?;
        let path = Path::new(&manifest_dir);
        Self::new(path, path)
    }

    #[allow(clippy::disallowed_types)]
    pub fn new(root: &Path, build_context: &Path) -> Result<Self> {
        let root_guard = GuardedPath::new_root(root)?;
        let build_guard = GuardedPath::new_root(build_context)?;
        #[cfg(miri)]
        let (root_state, build_state) = {
            let root_state = Rc::new(RefCell::new(SyntheticRootState::new()));
            if root_guard.as_path() == build_guard.as_path() {
                (root_state.clone(), root_state)
            } else {
                (root_state, Rc::new(RefCell::new(SyntheticRootState::new())))
            }
        };
        Ok(Self {
            root: root_guard,
            build_context: build_guard,
            #[cfg(miri)]
            root_state,
            #[cfg(miri)]
            build_state,
        })
    }

    pub fn root(&self) -> &GuardedPath {
        &self.root
    }

    pub fn build_context(&self) -> &GuardedPath {
        &self.build_context
    }

    pub fn set_root(&mut self, root: GuardedPath) {
        self.root = root;
    }

    #[cfg(miri)]
    fn state_for_guard(&self, guard: &GuardedPath) -> Rc<RefCell<SyntheticRootState>> {
        if guard.root() == self.root.as_path() {
            self.root_state.clone()
        } else {
            self.build_state.clone()
        }
    }

    #[cfg(miri)]
    fn normalize_rel(guard: &GuardedPath) -> String {
        let rel = guard
            .as_path()
            .strip_prefix(guard.root())
            .unwrap_or(guard.as_path())
            .to_path_buf();
        let mut parts: Vec<String> = Vec::new();
        for part in rel.components() {
            use std::path::Component;
            match part {
                Component::RootDir | Component::CurDir => {}
                Component::ParentDir => {
                    parts.pop();
                }
                Component::Normal(s) => parts.push(s.to_string_lossy().to_string()),
                Component::Prefix(p) => parts.push(p.as_os_str().to_string_lossy().to_string()),
            }
        }
        parts.join("/")
    }
}

#[cfg(miri)]
#[derive(Default)]
struct SyntheticRootState {
    files: HashMap<String, Vec<u8>>,
    dirs: HashSet<String>,
}

#[cfg(miri)]
impl SyntheticRootState {
    fn new() -> Self {
        let mut dirs = HashSet::new();
        dirs.insert(String::new());
        Self {
            files: HashMap::new(),
            dirs,
        }
    }

    fn ensure_dir(&mut self, rel: &str) {
        self.dirs.insert(String::new());
        if rel.is_empty() {
            return;
        }
        let mut current = Vec::new();
        for comp in rel.split('/') {
            if comp.is_empty() {
                continue;
            }
            current.push(comp.to_string());
            self.dirs.insert(current.join("/"));
        }
    }

    fn write_file(&mut self, rel: &str, contents: &[u8]) {
        if let Some((parent, _)) = rel.rsplit_once('/') {
            self.ensure_dir(parent);
        } else {
            self.ensure_dir("");
        }
        self.files.insert(rel.to_string(), contents.to_vec());
    }

    fn read_file(&self, rel: &str) -> Result<Vec<u8>> {
        self.files
            .get(rel)
            .cloned()
            .ok_or_else(|| anyhow::anyhow!("missing file {rel}"))
    }

    fn remove_file(&mut self, rel: &str) {
        self.files.remove(rel);
    }

    fn remove_dir_all(&mut self, rel: &str) {
        let prefix = if rel.is_empty() {
            String::new()
        } else {
            format!("{rel}/")
        };
        self.files
            .retain(|path, _| !path.eq(rel) && !path.starts_with(&prefix));
        self.dirs
            .retain(|dir| !dir.eq(rel) && !dir.starts_with(&prefix));
    }

    fn dir_exists(&self, rel: &str) -> bool {
        rel.is_empty() || self.dirs.contains(rel)
    }

    fn entry_kind(&self, rel: &str) -> Option<EntryKind> {
        if rel.is_empty() {
            return Some(EntryKind::Dir);
        }
        if self.files.contains_key(rel) {
            Some(EntryKind::File)
        } else if self.dirs.contains(rel) {
            Some(EntryKind::Dir)
        } else {
            None
        }
    }

    fn list_children(&self, rel: &str) -> Vec<(String, EntryKind)> {
        let mut entries: BTreeMap<String, EntryKind> = BTreeMap::new();
        let prefix = if rel.is_empty() {
            None
        } else {
            Some(format!("{rel}/"))
        };

        let mut push_child = |child: &str, kind: EntryKind| {
            if child.is_empty() {
                return;
            }
            entries
                .entry(child.to_string())
                .and_modify(|existing| {
                    if matches!(kind, EntryKind::Dir) {
                        *existing = EntryKind::Dir;
                    }
                })
                .or_insert(kind);
        };

        for dir in self.dirs.iter() {
            if rel.is_empty() {
                if dir.is_empty() {
                    continue;
                }
                if let Some(child) = dir.split('/').next() {
                    push_child(child, EntryKind::Dir);
                }
            } else if let Some(prefix) = prefix.as_ref() {
                if let Some(rest) = dir.strip_prefix(prefix) {
                    if let Some(child) = rest.split('/').next() {
                        push_child(child, EntryKind::Dir);
                    }
                }
            }
        }

        for file in self.files.keys() {
            if rel.is_empty() {
                if let Some(child) = file.split('/').next() {
                    push_child(child, EntryKind::File);
                }
            } else if let Some(prefix) = prefix.as_ref() {
                if let Some(rest) = file.strip_prefix(prefix) {
                    if let Some(child) = rest.split('/').next() {
                        push_child(child, EntryKind::File);
                    }
                }
            }
        }

        entries.into_iter().collect()
    }
}

pub(crate) mod access;
mod copy;
mod git;
mod io;
mod resolve;
#[cfg(not(miri))]
use access::guard_path;

#[cfg(feature = "mock-fs")]
pub mod mock;

// PathResolver is exported at crate root via `lib.rs` re-export.
