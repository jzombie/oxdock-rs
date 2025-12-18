#[cfg(feature = "embed")]
pub use rust_embed;

#[macro_export]
macro_rules! define_embed {
    ($name:ident, $folder:expr) => {
        #[cfg(not(miri))]
        use $crate::workspace_fs::embed::rust_embed;

        #[cfg(not(miri))]
        #[derive($crate::workspace_fs::embed::rust_embed::RustEmbed)]
        #[folder = $folder]
        // Force inclusion of all files, overriding .gitignore which might ignore the generated folder
        #[include = "**/*"]
        pub struct $name;

        #[cfg(miri)]
        pub struct $name;

        #[cfg(miri)]
        impl $name {
            pub fn get(
                _file: &str,
            ) -> Option<$crate::workspace_fs::embed::rust_embed::EmbeddedFile> {
                None
            }
            pub fn iter() -> impl Iterator<Item = std::borrow::Cow<'static, str>> {
                std::iter::empty()
            }
        }
    };
}
