fn main() {
    let feature = env!("CARGO_FEATURE_FIXTURE_FEATURE");
    assert_eq!(feature, "1");
    let target_os = env!("CARGO_CFG_TARGET_OS");
    assert!(!target_os.is_empty());
    let target_arch = env!("CARGO_CFG_TARGET_ARCH");
    assert!(!target_arch.is_empty());
}
