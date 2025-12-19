#![cfg_attr(not(feature = "std"), no_std)]

extern crate alloc;

pub mod rust_embed {
    use alloc::borrow::Cow;

    #[derive(Clone)]
    pub struct Metadata {
        hash: [u8; 32],
        last_modified: Option<u64>,
        created: Option<u64>,
    }

    impl Metadata {
        pub const fn __oxdock_new(
            hash: [u8; 32],
            last_modified: Option<u64>,
            created: Option<u64>,
        ) -> Self {
            Self {
                hash,
                last_modified,
                created,
            }
        }

        pub fn sha256_hash(&self) -> [u8; 32] {
            self.hash
        }

        pub fn last_modified(&self) -> Option<u64> {
            self.last_modified
        }

        pub fn created(&self) -> Option<u64> {
            self.created
        }
    }

    #[derive(Clone)]
    pub struct EmbeddedFile {
        pub data: Cow<'static, [u8]>,
        pub metadata: Metadata,
    }

    pub enum Filenames {
        Embedded(core::slice::Iter<'static, &'static str>),
    }

    impl Filenames {
        pub fn from_slice(slice: &'static [&'static str]) -> Self {
            Self::Embedded(slice.iter())
        }
    }

    impl Iterator for Filenames {
        type Item = Cow<'static, str>;

        fn next(&mut self) -> Option<Self::Item> {
            match self {
                Filenames::Embedded(iter) => iter.next().map(|s| Cow::Borrowed(*s)),
            }
        }
    }
}
