fn main() {
    if let Err(err) = oxdock_cli::run() {
        eprintln!("{err:?}");
        std::process::exit(1);
    }
}
