fn main() {
    // Define the `rust_analyzer` configuration option so we can use it in the code
    // to skip heavy operations (like executing scripts) during IDE analysis.
    println!("cargo:rustc-check-cfg=cfg(rust_analyzer)");

    // We need to track these environment variables so that Cargo rebuilds this script
    // (and re-evaluates the cfg) when switching between a normal terminal build
    // and a rust-analyzer build.
    println!("cargo:rerun-if-env-changed=RUSTC_WRAPPER");
    println!("cargo:rerun-if-env-changed=RUSTC_WORKSPACE_WRAPPER");

    // Detect if we are running under rust-analyzer.
    // rust-analyzer often sets RUSTC_WRAPPER or RUSTC_WORKSPACE_WRAPPER to point to itself
    // or a wrapper when running `cargo check` for diagnostics.
    if std::env::var("RUSTC_WRAPPER").map_or(false, |v| v.contains("rust-analyzer"))
        || std::env::var("RUSTC_WORKSPACE_WRAPPER").map_or(false, |v| v.contains("rust-analyzer"))
    {
        println!("cargo:rustc-cfg=rust_analyzer");
    }
}
