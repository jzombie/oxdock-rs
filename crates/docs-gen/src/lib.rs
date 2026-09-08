pub mod command_ref;
pub mod config;
pub mod discovery;
pub mod guard;
pub mod providers;
pub mod runner;
pub mod template_doc;

pub use config::{DocsGenConfig, StageSpec, TargetSpec};
pub use discovery::{DiscoveredTarget, discover_targets, member_for_dir};
pub use guard::validate_rel_path;
pub use providers::{CargoMetadata, CommandRefProvider, DataProvider, Fragment};
