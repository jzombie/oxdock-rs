fn main() {
    println!("cargo:rustc-check-cfg=cfg(rust_analyzer)");

    if std::env::var("RUSTC_WRAPPER").map_or(false, |v| v.contains("rust-analyzer"))
        || std::env::var("RUSTC_WORKSPACE_WRAPPER").map_or(false, |v| v.contains("rust-analyzer"))
    {
        println!("cargo:rustc-cfg=rust_analyzer");
    }
}
