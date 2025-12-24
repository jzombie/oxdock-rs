/// Small test helper to temporarily set an environment variable and restore
/// its previous value when dropped. Intended for use in unit/integration
/// tests across workspace crates.
pub struct EnvGuard {
    key: &'static str,
    value: Option<String>,
}

impl EnvGuard {
    /// Set `key` to `value`, returning a guard that restores the previous
    /// value (or removes the variable) when dropped.
    pub fn set(key: &'static str, value: &str) -> Self {
        let prev = std::env::var(key).ok();
        // SAFETY: environment mutation is considered unsafe in this workspace.
        // Tests must serialize access; this helper centralizes the unsafe block.
        unsafe {
            std::env::set_var(key, value);
        }
        Self { key, value: prev }
    }
}

impl Drop for EnvGuard {
    fn drop(&mut self) {
        match &self.value {
            Some(value) => unsafe { std::env::set_var(self.key, value) },
            None => unsafe { std::env::remove_var(self.key) },
        }
    }
}
