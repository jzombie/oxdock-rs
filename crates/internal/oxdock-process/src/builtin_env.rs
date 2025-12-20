use oxdock_fs::{GuardedPath, current_head_commit};
use std::collections::HashMap;

/// Built-in environment variables injected into the OxDock execution context.
#[derive(Debug, Default)]
pub struct BuiltinEnv {
    values: HashMap<String, String>,
}

impl BuiltinEnv {
    pub const WORKSPACE_GIT_COMMIT: &'static str = "WORKSPACE_GIT_COMMIT";

    pub fn collect(build_context: &GuardedPath) -> Self {
        let mut values = HashMap::new();
        if let Ok(Some(commit)) = current_head_commit(build_context) {
            values.insert(Self::WORKSPACE_GIT_COMMIT.to_string(), commit);
        }
        Self { values }
    }

    pub fn into_envs(self) -> HashMap<String, String> {
        self.values
    }
}
