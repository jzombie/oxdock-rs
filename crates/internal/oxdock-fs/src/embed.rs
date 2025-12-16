#[cfg(feature = "embed")]
pub use rust_embed;

#[macro_export]
macro_rules! define_embed {
    ($name:ident, $folder:expr) => {
        #[cfg(not(miri))]
        use $crate::embed::rust_embed;

        #[cfg(not(miri))]
        #[derive($crate::embed::rust_embed::RustEmbed)]
        #[folder = $folder]
        pub struct $name;

        #[cfg(miri)]
        pub struct $name;

        #[cfg(miri)]
        impl $name {
            pub fn get(_file: &str) -> Option<$crate::embed::rust_embed::EmbeddedFile> {
                None
            }
            pub fn iter() -> impl Iterator<Item = std::borrow::Cow<'static, str>> {
                std::iter::empty()
            }
        }
    };
}
