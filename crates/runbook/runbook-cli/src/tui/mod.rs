mod app;
pub(crate) mod clipboard;
pub(crate) mod config;
pub(crate) mod editor;
pub(crate) mod keymap;
pub(crate) mod layout;
pub(crate) mod logs;
pub(crate) mod utils;
pub(crate) mod views;

pub use app::run_tui;
