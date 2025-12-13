fn main() {
    if let Err(err) = doc_ox::run() {
        eprintln!("{err:?}");
        std::process::exit(1);
    }
}
