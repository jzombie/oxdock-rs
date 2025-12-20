use anyhow::Result;
use oxdock_process::CommandBuilder;

/// Emit `cargo:rustc-env=...` directives for enabled Cargo features.
pub fn emit_feature_envs() -> Result<()> {
    let mut features = Vec::new();
    for (key, value) in std::env::vars() {
        if key.starts_with("CARGO_FEATURE_") && value == "1" {
            println!("cargo:rustc-env={key}={value}");
            if let Some(name) = key.strip_prefix("CARGO_FEATURE_") {
                features.push(name.to_ascii_lowercase());
            }
        }
    }
    if !features.is_empty() {
        println!("cargo:rustc-env=CARGO_CFG_FEATURE={}", features.join(","));
    }
    Ok(())
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
    for line in stdout.lines() {
        let line = line.trim();
        if line.is_empty() {
            continue;
        }
        if let Some((key, rest)) = line.split_once('=') {
            let key = key.trim();
            let value = trim_cfg_quotes(rest.trim());
            println!(
                "cargo:rustc-env=CARGO_CFG_{}={}",
                key.to_ascii_uppercase(),
                value
            );
        } else {
            println!(
                "cargo:rustc-env=CARGO_CFG_{}=1",
                line.to_ascii_uppercase()
            );
        }
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
