// Simple shim crate that exposes the maintained `yaml-rust2` implementation
// under the original `yaml-rust` crate name so transitive dependencies can
// be satisfied via a workspace patch.

pub use yaml_rust2::*;
