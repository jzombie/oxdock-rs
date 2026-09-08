Includes the root `README.md` and `oxdock/README.md` as rustdoc doctests so the
Rust code fences in the documentation are compiled and executed on every
`cargo test` run. This catches documentation drift without a separate test
harness.

