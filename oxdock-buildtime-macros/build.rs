fn main() {
    println!("cargo:rustc-check-cfg=cfg(rust_analyzer)");

    // TODO: Use this check instead?
    //  if std::env::var("RUSTC_WRAPPER").map_or(false, |v| v.contains("rust-analyzer")) {
    //     println!("Running under rust-analyzer");
    //     // Add specific logic for rust-analyzer here
    // } else {
    //     println!("Not running under rust-analyzer, likely cargo build/check");
    // }
}
