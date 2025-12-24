use crate::GuardedPath;

/// Detect whether the current process can create filesystem symlinks under
/// the provided target directory. On Unix this is assumed to be available;
/// on Windows we attempt to create and remove a transient directory symlink.
pub fn can_create_symlinks(target: &GuardedPath) -> bool {
    #[cfg(unix)]
    {
        let _ = target;
        true
    }

    #[cfg(windows)]
    {
        use std::fs;
        use std::os::windows::fs::symlink_dir;
        let test_src = target.as_path().join("__oxdock_test_symlink_src");
        let test_dst = target.as_path().join("__oxdock_test_symlink_dst");
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
