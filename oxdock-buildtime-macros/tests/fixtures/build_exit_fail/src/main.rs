use oxdock_buildtime_macros::embed;

embed! {
    name: DemoAssets,
    script: "EXIT 5",
    out_dir: "prebuilt",
}

fn main() {
    let _ = DemoAssets::iter();
}
