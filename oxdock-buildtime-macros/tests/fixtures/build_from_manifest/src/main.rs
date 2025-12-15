use oxdock_buildtime_macros::embed;

mod demo_assets {
    use super::*;

    embed! {
        name: DemoAssets,
        script: "COPY source.txt copied.txt",
        out_dir: "prebuilt",
    }
}

use demo_assets::DemoAssets;

fn main() {
    // At runtime just check the embedded file exists.
    let data = DemoAssets::get("copied.txt").expect("copied file embedded");
    let s = std::str::from_utf8(data.data.as_ref()).expect("utf8");
    assert_eq!(s.trim_end(), "hello from manifest");
}
