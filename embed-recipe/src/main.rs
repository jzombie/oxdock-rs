fn main() {
    if let Err(err) = embed_recipe::run() {
        eprintln!("{err:?}");
        std::process::exit(1);
    }
}
