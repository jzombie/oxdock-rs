use anyhow::Result;
use oxdock_process::CommandBuilder;

/// Emit `cargo:rustc-env=...` directives for enabled Cargo features.
pub fn emit_feature_envs() -> Result<()> {
    let lines = feature_env_lines();
    for line in lines {
        println!("{line}");
    }
    Ok(())
}

fn feature_env_lines() -> Vec<String> {
    let mut features = Vec::new();
    let mut lines = Vec::new();
    for (key, value) in std::env::vars() {
        if key.starts_with("CARGO_FEATURE_") && value == "1" {
            lines.push(format!("cargo:rustc-env={key}={value}"));
            if let Some(name) = key.strip_prefix("CARGO_FEATURE_") {
                features.push(name.to_ascii_lowercase());
            }
        }
    }
    if !features.is_empty() {
        lines.push(format!(
            "cargo:rustc-env=CARGO_CFG_FEATURE={}",
            features.join(",")
        ));
    }
    lines
}

/// Emit `cargo:rustc-env=...` directives for cfg keys from `rustc --print cfg`.
pub fn emit_cfg_envs() -> Result<()> {
    let rustc = std::env::var("RUSTC").unwrap_or_else(|_| "rustc".to_string());
    let mut cmd = CommandBuilder::new(rustc);
    cmd.arg("--print").arg("cfg");
    if let Ok(target) = std::env::var("TARGET") {
        cmd.arg("--target").arg(target);
    }
    let output = cmd.output()?;
    if !output.status.success() {
        return Ok(());
    }
    let stdout = String::from_utf8_lossy(&output.stdout);
    let lines = cfg_env_lines(&stdout);
    for line in lines {
        println!("{line}");
    }
    Ok(())
}

/// Emit both feature and cfg env directives for downstream proc-macros.
pub fn emit_feature_and_cfg_envs() -> Result<()> {
    emit_feature_envs()?;
    emit_cfg_envs()?;
    Ok(())
}

fn trim_cfg_quotes(value: &str) -> &str {
    let value = value.trim();
    let value = value
        .strip_prefix("\\\"")
        .and_then(|s| s.strip_suffix("\\\""))
        .unwrap_or(value);
    let value = value
        .strip_prefix('"')
        .and_then(|s| s.strip_suffix('"'))
        .unwrap_or(value);
    value
        .strip_prefix('\'')
        .and_then(|s| s.strip_suffix('\''))
        .unwrap_or(value)
}

fn cfg_env_lines(output: &str) -> Vec<String> {
    let mut lines = Vec::new();
    for line in output.lines() {
        let line = line.trim();
        if line.is_empty() {
            continue;
        }
        if let Some((key, rest)) = line.split_once('=') {
            let key = key.trim();
            let value = trim_cfg_quotes(rest.trim());
            lines.push(format!(
                "cargo:rustc-env=CARGO_CFG_{}={}",
                key.to_ascii_uppercase(),
                value
            ));
        } else {
            lines.push(format!(
                "cargo:rustc-env=CARGO_CFG_{}=1",
                line.to_ascii_uppercase()
            ));
        }
    }
    lines
}

#[cfg(test)]
mod tests {
    use super::{
        cfg_env_lines, emit_cfg_envs, emit_feature_envs, feature_env_lines, trim_cfg_quotes,
    };
    use oxdock_fs::{GuardedPath, PathResolver};
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

    #[test]
    fn trims_cfg_quotes() {
        assert_eq!(trim_cfg_quotes("\"value\""), "value");
        assert_eq!(trim_cfg_quotes("\\\"value\\\""), "value");
        assert_eq!(trim_cfg_quotes("'value'"), "value");
        assert_eq!(trim_cfg_quotes("value"), "value");
    }

    #[test]
    fn feature_env_lines_collects_features() {
        let _a = EnvGuard::set("CARGO_FEATURE_ALPHA", "1");
        let _b = EnvGuard::set("CARGO_FEATURE_BETA", "1");
        let _c = EnvGuard::set("CARGO_FEATURE_IGNORE", "0");
        let lines = feature_env_lines();
        assert!(
            lines
                .iter()
                .any(|line| line == "cargo:rustc-env=CARGO_FEATURE_ALPHA=1")
        );
        assert!(
            lines
                .iter()
                .any(|line| line == "cargo:rustc-env=CARGO_FEATURE_BETA=1")
        );
        let features_line = lines
            .iter()
            .find(|line| line.starts_with("cargo:rustc-env=CARGO_CFG_FEATURE="))
            .expect("features line");
        let features = features_line
            .trim_start_matches("cargo:rustc-env=CARGO_CFG_FEATURE=")
            .split(',')
            .collect::<Vec<_>>();
        assert!(features.contains(&"alpha"));
        assert!(features.contains(&"beta"));
        let _ = emit_feature_envs();
    }

    #[test]
    fn cfg_env_lines_parses_output() {
        let output = r#"
            unix
            target_arch="x86_64"
            target_os='linux'
        "#;
        let lines = cfg_env_lines(output);
        assert!(lines.contains(&"cargo:rustc-env=CARGO_CFG_UNIX=1".to_string()));
        assert!(lines.contains(&"cargo:rustc-env=CARGO_CFG_TARGET_ARCH=x86_64".to_string()));
        assert!(lines.contains(&"cargo:rustc-env=CARGO_CFG_TARGET_OS=linux".to_string()));
    }

    #[cfg(unix)]
    #[cfg_attr(
        miri,
        ignore = "spawns a fake rustc script; Miri does not support process execution"
    )]
    #[test]
    fn emit_cfg_envs_runs_rustc_override() {
        let tempdir = GuardedPath::tempdir().expect("tempdir");
        let root = tempdir.as_guarded_path().clone();
        let resolver = PathResolver::new(root.as_path(), root.as_path()).expect("resolver");
        let script = root.join("fake-rustc.sh").expect("script path");
        let contents = b"#!/bin/sh\necho 'target_arch=\"x86_64\"'\necho unix\n";
        resolver
            .write_file(&script, contents)
            .expect("write script");
        resolver
            .set_permissions_mode_unix(&script, 0o755)
            .expect("chmod");
        let _rustc = EnvGuard::set("RUSTC", script.display().as_str());
        let _target = EnvGuard::set("TARGET", "x86_64-unknown-linux-gnu");
        emit_cfg_envs().expect("emit cfg envs");
    }
}
