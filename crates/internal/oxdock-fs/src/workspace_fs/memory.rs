use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::path::Path;
use std::rc::Rc;

use anyhow::{Result, bail};

use crate::WorkspaceFs;

use super::GuardedPath;

#[derive(Clone)]
pub struct MemoryWorkspaceFs {
    root: GuardedPath,
    build_context: GuardedPath,
    state: Rc<RefCell<MemoryState>>,
}

#[derive(Default)]
struct MemoryState {
    files: HashMap<String, Vec<u8>>,
    dirs: HashSet<String>,
}

impl MemoryWorkspaceFs {
    pub fn new() -> Self {
        let root = GuardedPath::new_root_from_str(".").unwrap();
        let build_context = root.clone();
        let mut dirs = HashSet::new();
        dirs.insert(String::new());
        Self {
            root,
            build_context,
            state: Rc::new(RefCell::new(MemoryState {
                files: HashMap::new(),
                dirs,
            })),
        }
    }

    pub fn snapshot(&self) -> HashMap<String, Vec<u8>> {
        self.state.borrow().files.clone()
    }

    fn normalize_rel(&self, base: &GuardedPath, rel: &str) -> Result<String> {
        let mut segments = if rel.starts_with('/') || rel.starts_with('\\') {
            Vec::new()
        } else {
            self.split_components(&self.relative_path(base))
        };
        for part in self.split_components(rel) {
            match part.as_str() {
                "" | "." => {}
                ".." => {
                    segments.pop();
                }
                other => segments.push(other.to_string()),
            }
        }
        Ok(segments.join("/"))
    }

    fn guard_from_rel(&self, rel: String) -> Result<GuardedPath> {
        if rel.is_empty() {
            return Ok(self.root.clone());
        }
        let native = if std::path::MAIN_SEPARATOR == '/' {
            rel
        } else {
            rel.replace('/', std::path::MAIN_SEPARATOR_STR)
        };
        self.root
            .join(native.as_str())
            .map_err(|e| anyhow::anyhow!(e.to_string()))
    }

    fn relative_path(&self, path: &GuardedPath) -> String {
        let rel = path
            .as_path()
            .strip_prefix(self.root.as_path())
            .unwrap_or_else(|_| path.as_path());
        let trimmed = rel
            .to_string_lossy()
            .trim_start_matches(std::path::MAIN_SEPARATOR)
            .to_string();
        trimmed.replace('\\', "/")
    }

    fn split_components(&self, input: &str) -> Vec<String> {
        input
            .split(['/', '\\'])
            .filter(|s| !s.is_empty())
            .map(|s| s.to_string())
            .collect()
    }
}

impl Default for MemoryWorkspaceFs {
    fn default() -> Self {
        Self::new()
    }
}

impl WorkspaceFs for MemoryWorkspaceFs {
    fn canonicalize_abs(&self, path: &GuardedPath) -> Result<GuardedPath> {
        Ok(path.clone())
    }

    fn metadata_abs(&self, _path: &GuardedPath) -> Result<std::fs::Metadata> {
        bail!("metadata not supported in memory fs");
    }

    #[allow(clippy::disallowed_types)]
    fn metadata_external(&self, _path: &crate::UnguardedPath) -> Result<std::fs::Metadata> {
        bail!("metadata not supported in memory fs");
    }

    fn root(&self) -> &GuardedPath {
        &self.root
    }

    fn build_context(&self) -> &GuardedPath {
        &self.build_context
    }

    fn set_root(&mut self, root: GuardedPath) {
        self.root = root;
    }

    fn read_file(&self, path: &GuardedPath) -> Result<Vec<u8>> {
        let rel = self.relative_path(path);
        self.state
            .borrow()
            .files
            .get(&rel)
            .cloned()
            .ok_or_else(|| anyhow::anyhow!("missing file {}", path.display()))
    }

    fn read_to_string(&self, path: &GuardedPath) -> Result<String> {
        let bytes = self.read_file(path)?;
        String::from_utf8(bytes).map_err(|e| anyhow::anyhow!(e))
    }

    fn read_dir_entries(&self, _path: &GuardedPath) -> Result<Vec<std::fs::DirEntry>> {
        bail!("read_dir unsupported in memory fs");
    }

    fn write_file(&self, path: &GuardedPath, contents: &[u8]) -> Result<()> {
        let rel = self.relative_path(path);
        self.state.borrow_mut().files.insert(rel, contents.to_vec());
        Ok(())
    }

    fn create_dir_all_abs(&self, path: &GuardedPath) -> Result<()> {
        let rel = self.relative_path(path);
        let mut state = self.state.borrow_mut();
        state.dirs.insert(String::new());
        let mut prefix: Vec<String> = Vec::new();
        for comp in self.split_components(&rel) {
            prefix.push(comp.clone());
            state.dirs.insert(prefix.join("/"));
        }
        Ok(())
    }

    fn remove_file_abs(&self, _path: &GuardedPath) -> Result<()> {
        bail!("remove unsupported")
    }

    fn remove_dir_all_abs(&self, _path: &GuardedPath) -> Result<()> {
        bail!("remove unsupported")
    }

    fn copy_file(&self, _src: &GuardedPath, _dst: &GuardedPath) -> Result<u64> {
        bail!("copy unsupported")
    }

    fn copy_dir_recursive(&self, _src: &GuardedPath, _dst: &GuardedPath) -> Result<()> {
        bail!("copy unsupported")
    }

    #[allow(clippy::disallowed_types)]
    fn copy_dir_from_external(
        &self,
        _src: &crate::UnguardedPath,
        _dst: &GuardedPath,
    ) -> Result<()> {
        bail!("copy unsupported")
    }

    #[allow(clippy::disallowed_types)]
    fn copy_file_from_external(
        &self,
        _src: &crate::UnguardedPath,
        _dst: &GuardedPath,
    ) -> Result<u64> {
        bail!("copy unsupported")
    }

    fn symlink(&self, _src: &GuardedPath, _dst: &GuardedPath) -> Result<()> {
        bail!("symlink unsupported")
    }

    #[allow(clippy::disallowed_types)]
    fn open_external_file(&self, _path: &crate::UnguardedPath) -> Result<std::fs::File> {
        bail!("open unsupported")
    }

    fn set_permissions_mode_unix(&self, _path: &GuardedPath, _mode: u32) -> Result<()> {
        bail!("perms unsupported")
    }

    fn resolve_workdir(&self, current: &GuardedPath, new_dir: &str) -> Result<GuardedPath> {
        let candidate = Path::new(new_dir);
        if candidate.is_absolute() {
            return GuardedPath::new(self.root.root(), candidate);
        }
        if new_dir == "/" {
            return Ok(self.root.clone());
        }
        let target = self.normalize_rel(current, new_dir)?;
        self.guard_from_rel(target)
    }

    fn resolve_read(&self, cwd: &GuardedPath, rel: &str) -> Result<GuardedPath> {
        let candidate = Path::new(rel);
        if candidate.is_absolute() {
            return GuardedPath::new(self.root.root(), candidate);
        }
        let target = self.normalize_rel(cwd, rel)?;
        self.guard_from_rel(target)
    }

    fn resolve_write(&self, cwd: &GuardedPath, rel: &str) -> Result<GuardedPath> {
        let candidate = Path::new(rel);
        if candidate.is_absolute() {
            return GuardedPath::new(self.root.root(), candidate);
        }
        let target = self.normalize_rel(cwd, rel)?;
        self.guard_from_rel(target)
    }

    fn resolve_copy_source(&self, from: &str) -> Result<GuardedPath> {
        let candidate = Path::new(from);
        if candidate.is_absolute() {
            return GuardedPath::new(self.root.root(), candidate);
        }
        let rel = self.split_components(from).join("/");
        self.guard_from_rel(rel)
    }

    fn copy_from_git(&self, _rev: &str, _from: &str, _to: &GuardedPath) -> Result<()> {
        bail!("git copy unsupported")
    }
}
