fn main() {
    if let Err(err) = doc_ox_cli::run() {
        eprintln!("{err:?}");
        std::process::exit(1);
    }
}
