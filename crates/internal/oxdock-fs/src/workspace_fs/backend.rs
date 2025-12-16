use anyhow::{bail, Context, Result};
use std::fs;

use super::GuardedPath;

/// Private trait describing the backend IO interface. Kept private to avoid
/// expanding the public API surface; used to ensure host/miri implementations
/// remain in sync.
trait BackendImpl {
    fn create_dir_all_abs(&self, root: &GuardedPath, path: &GuardedPath) -> Result<()>;
    fn read_dir_entries(&self, path: &GuardedPath) -> Result<Vec<super::DirEntry>>;
    fn read_file(&self, path: &GuardedPath) -> Result<Vec<u8>>;
    fn write_file(&self, path: &GuardedPath, contents: &[u8]) -> Result<()>;
    fn canonicalize_abs(&self, path: GuardedPath) -> Result<GuardedPath>;
    fn metadata_abs(&self, path: &GuardedPath) -> Result<std::fs::Metadata>;
    fn resolve_workdir(&self, resolved: GuardedPath) -> Result<GuardedPath>;
    fn resolve_copy_source(&self, guarded: GuardedPath) -> Result<GuardedPath>;
    fn remove_file_abs(&self, path: &GuardedPath) -> Result<()>;
    fn remove_dir_all_abs(&self, path: &GuardedPath) -> Result<()>;
}

// Host implementation (used when not under Miri)
#[cfg(not(miri))]
struct HostBackend;

#[cfg(not(miri))]
impl HostBackend {
    fn new(_root: &GuardedPath, _build: &GuardedPath) -> Result<Self> {
        Ok(Self {})
    }
}

#[cfg(not(miri))]
impl BackendImpl for HostBackend {
    fn create_dir_all_abs(&self, root: &GuardedPath, path: &GuardedPath) -> Result<()> {
        if !root.as_path().exists() {
            fs::create_dir_all(root.as_path())
                .with_context(|| format!("creating resolver root {}", root.display()))?;
        }
        fs::create_dir_all(path.as_path())
            .with_context(|| format!("creating dir {}", path.display()))?;
        Ok(())
    }

    fn read_dir_entries(&self, path: &GuardedPath) -> Result<Vec<super::DirEntry>> {
        let entries = fs::read_dir(path.as_path())
            .with_context(|| format!("failed to read dir {}", path.display()))?;
        let vec: Vec<std::fs::DirEntry> = entries.collect::<Result<_, _>>()?;
        Ok(vec)
    }

    fn read_file(&self, path: &GuardedPath) -> Result<Vec<u8>> {
        let data = fs::read(path.as_path()).with_context(|| format!("failed to read {}", path.display()))?;
        Ok(data)
    }

    fn write_file(&self, path: &GuardedPath, contents: &[u8]) -> Result<()> {
        if let Some(parent) = path.as_path().parent() {
            fs::create_dir_all(parent)
                .with_context(|| format!("creating dir {}", parent.display()))?;
        }
        fs::write(path.as_path(), contents)
            .with_context(|| format!("writing {}", path.display()))?;
        Ok(())
    }

    fn canonicalize_abs(&self, path: GuardedPath) -> Result<GuardedPath> {
        Ok(path)
    }

    fn metadata_abs(&self, path: &GuardedPath) -> Result<std::fs::Metadata> {
        let m = fs::metadata(path.as_path())
            .with_context(|| format!("failed to stat {}", path.display()))?;
        Ok(m)
    }

    fn resolve_workdir(&self, resolved: GuardedPath) -> Result<GuardedPath> {
        if let Ok(meta) = fs::metadata(resolved.as_path()) {
            if meta.is_dir() {
                let canon = fs::canonicalize(resolved.as_path())
                    .unwrap_or_else(|_| resolved.to_path_buf());
                return GuardedPath::new(resolved.root(), &canon);
            }
            bail!("WORKDIR path is not a directory: {}", resolved.display());
        }

        fs::create_dir_all(resolved.as_path())
            .with_context(|| format!("failed to create WORKDIR {}", resolved.display()))?;
        let final_abs = fs::canonicalize(resolved.as_path()).unwrap_or_else(|_| resolved.to_path_buf());
        GuardedPath::new(resolved.root(), &final_abs)
    }

    fn resolve_copy_source(&self, guarded: GuardedPath) -> Result<GuardedPath> {
        if !guarded.as_path().exists() {
            bail!("COPY source missing in build context: {}", guarded.display());
        }
        Ok(guarded)
    }

    fn remove_file_abs(&self, path: &GuardedPath) -> Result<()> {
        match std::fs::remove_file(path.as_path()) {
            Ok(_) => {}
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => {}
            Err(e) => {
                return Err(e)
                    .with_context(|| format!("failed to remove file {}", path.display()));
            }
        }
        Ok(())
    }

    fn remove_dir_all_abs(&self, path: &GuardedPath) -> Result<()> {
        std::fs::remove_dir_all(path.as_path())
            .with_context(|| format!("failed to remove dir {}", path.display()))?;
        Ok(())
    }
}

// Miri implementation (keeps synthetic state + mirrors to host)
#[cfg(miri)]
mod miri_backend {
    use super::*;
    use anyhow::anyhow;
    use std::collections::{BTreeMap, HashMap, HashSet};
    use std::path::Component;

    pub(super) struct MiriBackend {
        root_state: std::rc::Rc<std::cell::RefCell<SyntheticRootState>>,
        build_state: std::rc::Rc<std::cell::RefCell<SyntheticRootState>>,
        root_path: std::path::PathBuf,
    }

    impl MiriBackend {
        pub(super) fn new(root: &GuardedPath, build: &GuardedPath) -> Result<Self> {
            let root_state = std::rc::Rc::new(std::cell::RefCell::new(SyntheticRootState::new()));
            seed_from_host(root, &root_state)?;

            if root.as_path() == build.as_path() {
                Ok(Self {
                    root_state: root_state.clone(),
                    build_state: root_state,
                    root_path: root.as_path().to_path_buf(),
                })
            } else {
                let build_state = std::rc::Rc::new(std::cell::RefCell::new(SyntheticRootState::new()));
                seed_from_host(build, &build_state)?;
                Ok(Self {
                    root_state,
                    build_state,
                    root_path: root.as_path().to_path_buf(),
                })
            }
        }

        fn state_for_guard_rc(&self, guard: &GuardedPath) -> std::rc::Rc<std::cell::RefCell<SyntheticRootState>> {
            if guard.root() == self.root_path.as_path() {
                self.root_state.clone()
            } else {
                self.build_state.clone()
            }
        }
    }

    impl BackendImpl for MiriBackend {
        fn create_dir_all_abs(&self, _root: &GuardedPath, path: &GuardedPath) -> Result<()> {
            let state = self.state_for_guard_rc(path);
            let rel = normalize_rel(path);
            state.borrow_mut().ensure_dir(&rel);
            if let Some(parent) = path.as_path().parent() {
                fs::create_dir_all(parent)
                    .with_context(|| format!("creating dir {}", parent.display()))?;
            }
            fs::create_dir_all(path.as_path())
                .with_context(|| format!("creating dir {}", path.display()))?;
            Ok(())
        }

        fn read_dir_entries(&self, path: &GuardedPath) -> Result<Vec<super::DirEntry>> {
            if path.as_path().exists() {
                let entries = fs::read_dir(path.as_path())
                    .with_context(|| format!("failed to read dir {}", path.display()))?;
                let mut out: Vec<super::DirEntry> = Vec::new();
                for entry in entries {
                    let entry = entry?;
                    let ft = entry.file_type()?;
                    let kind = if ft.is_dir() { super::EntryKind::Dir } else { super::EntryKind::File };
                    out.push(super::DirEntry::new(entry.path(), kind));
                }
                return Ok(out);
            }

            let rel = normalize_rel(path);
            let state_ref = self.state_for_guard_rc(path);
            let state = state_ref.borrow();
            if !state.dir_exists(&rel) {
                bail!("directory does not exist: {}", path.display());
            }
            let children = state.list_children(&rel);
            Ok(children
                .into_iter()
                .map(|(name, kind)| super::DirEntry::new(path.as_path().join(name), kind))
                .collect())
        }

        fn read_file(&self, path: &GuardedPath) -> Result<Vec<u8>> {
            if path.as_path().exists() {
                let data = fs::read(path.as_path())
                    .with_context(|| format!("failed to read {}", path.display()))?;
                return Ok(data);
            }
            let rel = normalize_rel(path);
            let data = self.state_for_guard_rc(path).borrow().read_file(&rel)?;
            Ok(data)
        }

        fn write_file(&self, path: &GuardedPath, contents: &[u8]) -> Result<()> {
            let rel = normalize_rel(path);
            let binding = self.state_for_guard_rc(path);
            binding.borrow_mut().write_file(&rel, contents);
            if let Some(parent) = path.as_path().parent() {
                fs::create_dir_all(parent)
                    .with_context(|| format!("creating dir {}", parent.display()))?;
            }
            fs::write(path.as_path(), contents)
                .with_context(|| format!("writing {}", path.display()))?;
            Ok(())
        }

        fn canonicalize_abs(&self, path: GuardedPath) -> Result<GuardedPath> {
            let canon = fs::canonicalize(path.as_path()).unwrap_or_else(|_| path.to_path_buf());
            GuardedPath::new(path.root(), &canon)
        }

        fn metadata_abs(&self, path: &GuardedPath) -> Result<std::fs::Metadata> {
            let m = fs::metadata(path.as_path())
                .with_context(|| format!("failed to stat {}", path.display()))?;
            Ok(m)
        }

        fn resolve_workdir(&self, resolved: GuardedPath) -> Result<GuardedPath> {
            let rel = normalize_rel(&resolved);
            let state = self.state_for_guard_rc(&resolved);

            if resolved.as_path().exists() {
                let meta = fs::metadata(resolved.as_path())
                    .with_context(|| format!("failed to stat {}", resolved.display()))?;
                if !meta.is_dir() {
                    bail!("WORKDIR path is not a directory: {}", resolved.display());
                }
                {
                    let mut state_mut = state.borrow_mut();
                    if state_mut.entry_kind(&rel).is_none() {
                        state_mut.ensure_dir(&rel);
                    }
                }
            } else {
                {
                    let mut state_mut = state.borrow_mut();
                    if state_mut.entry_kind(&rel).is_none() {
                        state_mut.ensure_dir(&rel);
                    }
                }
                if let Some(parent) = resolved.as_path().parent() {
                    fs::create_dir_all(parent)
                        .with_context(|| format!("creating dir {}", parent.display()))?;
                }
                fs::create_dir_all(resolved.as_path())
                    .with_context(|| format!("creating WORKDIR {}", resolved.display()))?;
            }

            let final_abs = fs::canonicalize(resolved.as_path()).unwrap_or_else(|_| resolved.to_path_buf());
            GuardedPath::new(resolved.root(), &final_abs)
        }

        fn resolve_copy_source(&self, guarded: GuardedPath) -> Result<GuardedPath> {
            let rel = normalize_rel(&guarded);
            let kind = self
                .state_for_guard_rc(&guarded)
                .borrow()
                .entry_kind(&rel)
                .ok_or_else(|| anyhow!("COPY source missing in build context: {}", guarded.display()))?;
            if matches!(kind, super::EntryKind::Dir | super::EntryKind::File) {
                Ok(guarded)
            } else {
                bail!("COPY source unsupported kind: {}", guarded.display());
            }
        }

        fn remove_file_abs(&self, path: &GuardedPath) -> Result<()> {
            let rel = normalize_rel(path);
            self.state_for_guard_rc(path)
                .borrow_mut()
                .remove_file(&rel);
            match std::fs::remove_file(path.as_path()) {
                Ok(_) => {}
                Err(e) if e.kind() == std::io::ErrorKind::NotFound => {}
                Err(e) => {
                    return Err(e)
                        .with_context(|| format!("failed to remove file {}", path.display()));
                }
            }
            Ok(())
        }

        fn remove_dir_all_abs(&self, path: &GuardedPath) -> Result<()> {
            let rel = normalize_rel(path);
            self.state_for_guard_rc(path)
                .borrow_mut()
                .remove_dir_all(&rel);
            let _ = std::fs::remove_dir_all(path.as_path());
            Ok(())
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

    #[cfg(miri)]
    fn seed_from_host(root: &GuardedPath, state: &std::rc::Rc<std::cell::RefCell<SyntheticRootState>>) -> Result<()> {
        if !root.as_path().exists() {
            return Ok(());
        }

        fn walk(dir: &std::path::Path, rel_prefix: &str, state: &mut SyntheticRootState) {
            if let Ok(entries) = fs::read_dir(dir) {
                for entry in entries.flatten() {
                    let path = entry.path();
                    let name = entry.file_name().to_string_lossy().to_string();
                    let rel = if rel_prefix.is_empty() {
                        name.clone()
                    } else {
                        format!("{}/{}", rel_prefix, name)
                    };
                    if let Ok(ft) = entry.file_type() {
                        if ft.is_dir() {
                            state.ensure_dir(&rel);
                            walk(&path, &rel, state);
                        } else if ft.is_file() {
                            if let Ok(data) = fs::read(&path) {
                                state.write_file(&rel, &data);
                            }
                        }
                    }
                }
            }
        }

        walk(root.as_path(), "", &mut state.borrow_mut());
        Ok(())
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
                .ok_or_else(|| anyhow!("missing file {rel}"))
        }

        fn remove_file(&mut self, rel: &str) {
            self.files.remove(rel);
        }

        fn remove_dir_all(&mut self, rel: &str) {
            let prefix = if rel.is_empty() { String::new() } else { format!("{rel}/") };
            self.files.retain(|path, _| !path.eq(rel) && !path.starts_with(&prefix));
            self.dirs.retain(|dir| !dir.eq(rel) && !dir.starts_with(&prefix));
        }

        fn dir_exists(&self, rel: &str) -> bool {
            rel.is_empty() || self.dirs.contains(rel)
        }

        fn entry_kind(&self, rel: &str) -> Option<super::EntryKind> {
            if rel.is_empty() {
                return Some(super::EntryKind::Dir);
            }
            if self.files.contains_key(rel) {
                Some(super::EntryKind::File)
            } else if self.dirs.contains(rel) {
                Some(super::EntryKind::Dir)
            } else {
                None
            }
        }

        fn list_children(&self, rel: &str) -> Vec<(String, super::EntryKind)> {
            let mut entries: BTreeMap<String, super::EntryKind> = BTreeMap::new();
            let prefix = if rel.is_empty() { None } else { Some(format!("{rel}/")) };

            let mut push_child = |child: &str, kind: super::EntryKind| {
                if child.is_empty() {
                    return;
                }
                entries
                    .entry(child.to_string())
                    .and_modify(|existing| {
                        if matches!(kind, super::EntryKind::Dir) {
                            *existing = super::EntryKind::Dir;
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
                        push_child(child, super::EntryKind::Dir);
                    }
                } else if let Some(prefix) = prefix.as_ref() {
                    if let Some(rest) = dir.strip_prefix(prefix) {
                        if let Some(child) = rest.split('/').next() {
                            push_child(child, super::EntryKind::Dir);
                        }
                    }
                }
            }

            for file in self.files.keys() {
                if rel.is_empty() {
                    if let Some(child) = file.split('/').next() {
                        push_child(child, super::EntryKind::File);
                    }
                } else if let Some(prefix) = prefix.as_ref() {
                    if let Some(rest) = file.strip_prefix(prefix) {
                        if let Some(child) = rest.split('/').next() {
                            push_child(child, super::EntryKind::File);
                        }
                    }
                }
            }

            entries.into_iter().collect()
        }
    }
}

/// Public backend delegator used by `PathResolver`. It holds either a host
/// or miri backend and forwards calls to the concrete implementation.
pub(super) enum Backend {
    #[cfg(not(miri))]
    Host(HostBackend),
    #[cfg(miri)]
    Miri(miri_backend::MiriBackend),
}

impl Backend {
    pub(super) fn new(root: &GuardedPath, build: &GuardedPath) -> Result<Self> {
        #[cfg(miri)] {
            return Ok(Backend::Miri(miri_backend::MiriBackend::new(root, build)?));
        }
        #[cfg(not(miri))] {
            return Ok(Backend::Host(HostBackend::new(root, build)?));
        }
    }

    fn as_impl(&self) -> &dyn BackendImpl {
        match self {
            #[cfg(not(miri))]
            Backend::Host(h) => h,
            #[cfg(miri)]
            Backend::Miri(m) => m,
        }
    }

    pub(super) fn create_dir_all_abs(&self, root: &GuardedPath, path: &GuardedPath) -> Result<()> {
        self.as_impl().create_dir_all_abs(root, path)
    }

    pub(super) fn read_dir_entries(&self, path: &GuardedPath) -> Result<Vec<super::DirEntry>> {
        self.as_impl().read_dir_entries(path)
    }

    pub(super) fn read_file(&self, path: &GuardedPath) -> Result<Vec<u8>> {
        self.as_impl().read_file(path)
    }

    pub(super) fn write_file(&self, path: &GuardedPath, contents: &[u8]) -> Result<()> {
        self.as_impl().write_file(path, contents)
    }

    pub(super) fn canonicalize_abs(&self, path: GuardedPath) -> Result<GuardedPath> {
        self.as_impl().canonicalize_abs(path)
    }

    pub(super) fn metadata_abs(&self, path: &GuardedPath) -> Result<std::fs::Metadata> {
        self.as_impl().metadata_abs(path)
    }

    pub(super) fn resolve_workdir(&self, resolved: GuardedPath) -> Result<GuardedPath> {
        self.as_impl().resolve_workdir(resolved)
    }

    pub(super) fn resolve_copy_source(&self, guarded: GuardedPath) -> Result<GuardedPath> {
        self.as_impl().resolve_copy_source(guarded)
    }

    pub(super) fn remove_file_abs(&self, path: &GuardedPath) -> Result<()> {
        self.as_impl().remove_file_abs(path)
    }

    pub(super) fn remove_dir_all_abs(&self, path: &GuardedPath) -> Result<()> {
        self.as_impl().remove_dir_all_abs(path)
    }
}

