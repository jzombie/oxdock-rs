fn main() {
    // Let clippy/rustc know about the analyzer-specific cfg we rely on for skipping execution
    // when rust-analyzer expands the macro inside the IDE.
    println!("cargo:rustc-check-cfg=cfg(rust_analyzer)");
    if std::env::var_os("RA_PROC_MACRO_SERVER").is_some() {
        // rust-analyzer's proc-macro server sets this env var, so we mirror it back as a cfg
        // that the actual crate code can branch on without poking environment variables again.
        println!("cargo:rustc-cfg=rust_analyzer");
    }
}
