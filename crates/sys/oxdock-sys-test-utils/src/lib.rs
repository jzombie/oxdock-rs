/// Shared test helpers used by multiple crates' tests.
///
/// Keep functionality here minimal and test-only.
use std::env;

pub struct TestEnvGuard {
    key: &'static str,
    value: Option<String>,
}

impl TestEnvGuard {
    pub fn set(key: &'static str, value: &str) -> Self {
        let prev = env::var(key).ok();
        unsafe { env::set_var(key, value) };
        Self { key, value: prev }
    }
}

impl Drop for TestEnvGuard {
    fn drop(&mut self) {
        match &self.value {
            Some(value) => unsafe { env::set_var(self.key, value) },
            None => unsafe { env::remove_var(self.key) },
        }
    }
}

#[allow(clippy::disallowed_types)]
use std::path::Path;

/// Detect whether the current process can create filesystem symlinks under
/// the provided target directory. Accepts a `&Path` to avoid depending on
/// `oxdock-fs` and creating a circular crate dependency.
#[allow(clippy::disallowed_types)]
pub fn can_create_symlinks(target: &Path) -> bool {
    #[cfg(unix)]
    {
        let _ = target;
        true
    }

    #[cfg(windows)]
    {
        use std::fs;
        use std::os::windows::fs::symlink_dir;
        let test_src = target.join("__oxdock_test_symlink_src");
        let test_dst = target.join("__oxdock_test_symlink_dst");
        let _ = fs::create_dir_all(&test_src);
        let ok = symlink_dir(&test_src, &test_dst).is_ok();
        let _ = fs::remove_dir_all(&test_dst);
        let _ = fs::remove_dir_all(&test_src);
        ok
    }

    #[cfg(not(any(unix, windows)))]
    {
        let _ = target;
        false
    }
}
