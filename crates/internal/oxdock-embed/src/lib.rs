use anyhow::{Context, Result};
use oxdock_fs::{GuardedPath, PathResolver};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use sha2::{Digest, Sha256};
use std::time::SystemTime;

pub mod rust_embed {
    extern crate alloc;
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

#[derive(Clone)]
pub struct AssetRecord {
    pub rel_path: String,
    pub include_path: String,
    pub sha256: [u8; 32],
    pub last_modified: Option<u64>,
    pub created: Option<u64>,
}

pub fn gather_assets(
    resolver: &PathResolver,
    asset_root: &GuardedPath,
) -> Result<Vec<AssetRecord>> {
    let mut assets = Vec::new();
    collect_assets_recursive(resolver, asset_root, asset_root, &mut assets)?;
    assets.sort_by(|a, b| a.rel_path.cmp(&b.rel_path));
    Ok(assets)
}

fn collect_assets_recursive(
    resolver: &PathResolver,
    root: &GuardedPath,
    dir: &GuardedPath,
    assets: &mut Vec<AssetRecord>,
) -> Result<()> {
    let mut entries = resolver
        .read_dir_entries(dir)
        .with_context(|| format!("failed to read assets in {}", dir.as_path().display()))?;
    entries.sort_by(|a, b| a.path().cmp(&b.path()));

    for entry in entries {
        let entry_path = entry.path();
        let entry_guard = GuardedPath::new(root.root(), entry_path.as_path())
            .with_context(|| format!("failed to guard entry {}", entry_path.as_path().display()))?;
        let file_type = entry.file_type().with_context(|| {
            format!(
                "failed to read file type for {}",
                entry_guard.as_path().display()
            )
        })?;

        if file_type.is_dir() {
            collect_assets_recursive(resolver, root, &entry_guard, assets)?;
            continue;
        }

        if !file_type.is_file() {
            continue;
        }

        let rel_path = entry_guard
            .as_path()
            .strip_prefix(root.as_path())
            .with_context(|| {
                format!(
                    "embedded file {} not under {}",
                    entry_guard.as_path().display(),
                    root.as_path().display()
                )
            })?;
        let rel_str = rel_path.to_str().with_context(|| {
            format!(
                "embedded file path is not valid UTF-8: {}",
                entry_guard.as_path().display()
            )
        })?;
        let rel_forward = oxdock_fs::to_forward_slashes(rel_str);
        let include_path = oxdock_fs::embed_path(&entry_guard);

        let bytes = resolver
            .read_file(&entry_guard)
            .with_context(|| format!("failed to read {} for hashing", rel_forward))?;
        let mut hasher = Sha256::new();
        hasher.update(&bytes);
        let sha256: [u8; 32] = hasher.finalize().into();

        let metadata = resolver
            .metadata(&entry_guard)
            .with_context(|| format!("failed to read metadata for {}", rel_forward))?;
        let last_modified = metadata
            .modified()
            .ok()
            .and_then(|mtime| mtime.duration_since(SystemTime::UNIX_EPOCH).ok())
            .map(|d| d.as_secs());
        let created = metadata
            .created()
            .ok()
            .and_then(|ctime| ctime.duration_since(SystemTime::UNIX_EPOCH).ok())
            .map(|d| d.as_secs());

        assets.push(AssetRecord {
            rel_path: rel_forward,
            include_path,
            sha256,
            last_modified,
            created,
        });
    }

    Ok(())
}

pub fn emit_embed_module(name: &syn::Ident, assets: &[AssetRecord]) -> syn::Result<TokenStream> {
    let mod_ident = format_ident!("__oxdock_embed_{}", name);
    let bytes_idents: Vec<_> = (0..assets.len())
        .map(|idx| format_ident!("__OXDOCK_EMBED_BYTES_{idx}"))
        .collect();
    let abs_paths: Vec<_> = assets
        .iter()
        .map(|asset| syn::LitStr::new(&asset.include_path, proc_macro2::Span::call_site()))
        .collect();
    let rel_paths: Vec<_> = assets
        .iter()
        .map(|asset| syn::LitStr::new(&asset.rel_path, proc_macro2::Span::call_site()))
        .collect();

    let bytes_consts = bytes_idents
        .iter()
        .zip(abs_paths.iter())
        .map(|(ident, abs)| quote! { const #ident: &[u8] = include_bytes!(#abs); });

    let metadata_tokens: Vec<_> = assets
        .iter()
        .map(|asset| {
            let hash_bytes: Vec<_> = asset
                .sha256
                .iter()
                .map(|b| {
                    let lit = proc_macro2::Literal::u8_unsuffixed(*b);
                    quote! { #lit }
                })
                .collect();
            let last_modified = match asset.last_modified {
                Some(v) => {
                    let lit = syn::LitInt::new(&v.to_string(), proc_macro2::Span::call_site());
                    quote! { Some(#lit) }
                }
                None => quote! { None },
            };
            let created = match asset.created {
                Some(v) => {
                    let lit = syn::LitInt::new(&v.to_string(), proc_macro2::Span::call_site());
                    quote! { Some(#lit) }
                }
                None => quote! { None },
            };
            quote! {
                Metadata::__oxdock_new(
                    [#(#hash_bytes),*],
                    #last_modified,
                    #created
                )
            }
        })
        .collect();

    let asset_entries: Vec<_> = rel_paths
        .iter()
        .zip(bytes_idents.iter())
        .zip(metadata_tokens.iter())
        .map(|((rel, ident), metadata)| {
            quote! {
                AssetEntry {
                    rel: #rel,
                    data: #ident,
                    metadata: #metadata,
                }
            }
        })
        .collect();

    Ok(quote! {
        #[allow(clippy::disallowed_methods, clippy::disallowed_types)]
        mod #mod_ident {
            extern crate alloc;

            use alloc::borrow::Cow;
            use oxdock_embed::rust_embed::{EmbeddedFile, Filenames, Metadata};

            #[derive(Clone)]
            struct AssetEntry {
                rel: &'static str,
                data: &'static [u8],
                metadata: Metadata,
            }

            #( #bytes_consts )*
            const __OXDOCK_EMBED_FILENAMES: &[&str] = &[
                #(#rel_paths),*
            ];
            const __OXDOCK_EMBED_ASSETS: &[AssetEntry] = &[
                #(#asset_entries),*
            ];

            pub struct #name;

            impl #name {
                pub fn get(path: &str) -> Option<EmbeddedFile> {
                    __OXDOCK_EMBED_ASSETS
                        .iter()
                        .find(|entry| entry.rel == path)
                        .map(|entry| EmbeddedFile {
                            data: Cow::Borrowed(entry.data),
                            metadata: entry.metadata.clone(),
                        })
                }

                pub fn iter() -> Filenames {
                    Filenames::from_slice(__OXDOCK_EMBED_FILENAMES)
                }
            }
        }

        pub use #mod_ident::#name;
    })
}
