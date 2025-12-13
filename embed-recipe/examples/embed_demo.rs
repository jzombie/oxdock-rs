use embed_recipe_macros::embed_dsl;

embed_dsl! {
    name: DemoAssets,
    script: r#"
      WORKDIR /
      MKDIR assets
      MKDIR assets/dir
      WRITE assets/hello.txt hello from embed
      WRITE assets/dir/nested.txt nested file
      RUN pwd > current_directory.txt
      COPY Cargo.toml assets/dir/copied.txt
    "#,
    // When built from crates.io (no .git) or as a non-primary package, fall back to this prebuilt folder.
    prebuilt: "examples/prebuilt_assets",
}

fn main() {
    println!("\nIterating embedded assets via DemoAssets::iter():");
    for f in DemoAssets::iter() {
        let name = f.as_ref();
        if let Some(file) = DemoAssets::get(name) {
            let contents = String::from_utf8_lossy(file.data.as_ref());
            println!(" embedded: {} -> {}", name, contents);
        }
    }
}
