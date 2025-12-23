use oxdock_buildtime_macros::embed;

mod demo_assets {
    use super::*;

    embed! {
        name: DemoAssets,
        script: "CAPTURE_TO_FILE env.txt ECHO {{ env:CARGO_FEATURE_OXDOCK_TEST }}:{{ env:CARGO_CFG_TARGET_OS }}",
        out_dir: "prebuilt",
    }
}

use demo_assets::DemoAssets;

fn main() {
    let data = DemoAssets::get("env.txt").expect("env.txt should be embedded");
    let contents = std::str::from_utf8(data.data.as_ref()).expect("env.txt should be utf8");
    let mut parts = contents.trim().split(':');
    let feature = parts.next().unwrap_or_default();
    let target_os = parts.next().unwrap_or_default();
    assert_eq!(feature, "1");
    assert!(!target_os.is_empty(), "target os should be present");
}
