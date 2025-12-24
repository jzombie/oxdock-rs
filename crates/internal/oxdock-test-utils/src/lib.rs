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
