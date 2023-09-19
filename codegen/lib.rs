use std::{
    borrow::Cow,
    env::VarError,
    fs,
    io::Write,
    path::PathBuf,
    sync::{
        atomic::{AtomicU32, Ordering::SeqCst},
        Mutex,
    },
};

use muttest_core::{
    context::{ENV_VAR_MUTTEST_DIR, MUTABLE_DEFINITIONS_CSV_HEAD},
    report::CrateId,
    transformer::{
        test_transformer::MuttestTestTransformer, MutablesConf, MuttestTransformer, TransformerConf,
    },
    Error,
};
use once_cell::sync::Lazy;
use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::{quote, ToTokens};
use syn::{fold::Fold, parse_macro_input, File};

static MUTTEST_DIR: Lazy<Option<PathBuf>> =
    Lazy::new(|| match std::env::var(ENV_VAR_MUTTEST_DIR) {
        Ok(d) => Some(PathBuf::from(d)),
        Err(VarError::NotPresent) => None,
        Err(e) => panic!("{}", e),
        // TODO: internal error instead
    });
static CRATE_ID: Lazy<CrateId> = Lazy::new(|| CrateId {
    pkg_name: Cow::Owned(std::env::var("CARGO_PKG_NAME").expect("unable to get env var")),
    crate_name: Cow::Owned(std::env::var("CARGO_CRATE_NAME").expect("unable to get env var")),
});
static MUTABLE_DEFINITIONS_FILE: Lazy<Mutex<Option<fs::File>>> =
    Lazy::new(|| Mutex::new(open_definitions_file().expect("unable to open definitions file")));
static ATTR_ID: AtomicU32 = AtomicU32::new(1);
static TEST_ATTR_ID: AtomicU32 = AtomicU32::new(1);

fn open_definitions_file() -> Result<Option<fs::File>, Error> {
    match &*MUTTEST_DIR {
        Some(dir) => {
            let file_name = format!("mutable-definitions-{}.csv", &*CRATE_ID);
            let mut file = fs::File::create(dir.join(file_name))?;
            write!(&mut file, "{MUTABLE_DEFINITIONS_CSV_HEAD}")?;
            file.flush()?;
            file.sync_all()?;
            Ok(Some(file))
        }
        _ => Ok(None),
    }
}

// TODO: somehow test that

/// Macro to enable mutation testing.
#[cfg(feature = "mutate")]
#[proc_macro_attribute]
pub fn mutate(_attr: TokenStream, input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as File);

    let attr_id = ATTR_ID.fetch_add(1, SeqCst);
    let conf = TransformerConf {
        attr_id,
        span: Span::call_site(),
        muttest_api: quote!(::muttest::api),
        crate_id: CRATE_ID.clone(),
    };

    let mut transformer = MuttestTransformer::new(conf, MutablesConf::All);
    let mut result = transformer.fold_file(input);
    if !transformer.errors.is_empty() {
        // TODO: emit compiler errors instead of panic
        panic!("{:?}", transformer.errors)
    }
    result.items.push(transformer.make_span_test());

    if let Some(f) = &mut *MUTABLE_DEFINITIONS_FILE.lock().unwrap() {
        transformer
            .write_csv(csv::Writer::from_writer(f))
            .expect("unable to write definitions csv");
    }

    result.into_token_stream().into()
}

/// Macro to enable mutation testing.
#[cfg(feature = "mutate")]
#[proc_macro_attribute]
pub fn tests(_attr: TokenStream, input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as syn::Item);

    let result = MuttestTestTransformer {
        conf: TransformerConf {
            attr_id: TEST_ATTR_ID.fetch_add(1, SeqCst),
            span: Span::call_site(),
            muttest_api: quote!(::muttest::api),
            crate_id: CRATE_ID.clone(),
        },
        test_count: 0,
    }
    .fold_item(input);

    result.into_token_stream().into()
}

/// isolated mutation for unit testing of `muttest-core` itself.
#[cfg(feature = "selftest")]
#[proc_macro_attribute]
pub fn mutate_isolated(attr: TokenStream, input: TokenStream) -> TokenStream {
    // static shared across invocations
    static ISOLATED_MUTATION: AtomicU32 = AtomicU32::new(1);

    // verify that this macro is only called from the intended packages

    use syn::parse_quote;
    let pkg_name = std::env::var("CARGO_PKG_NAME").unwrap();
    if pkg_name != "muttest-core" && pkg_name != "muttest-selftest" {
        panic!("`mutate_isolated` should only be used for internal testing");
    }

    let input = parse_macro_input!(input as syn::Item);

    // check c
    match &input {
        syn::Item::Fn(_) => {}
        syn::Item::Mod(m) if m.content.is_none() => {
            panic!("`mutate_isolated` cannot mutate module declarations")
        }
        syn::Item::Mod(_) => {}
        _ => panic!("`mutate_isolated` can only be used for fns and mods"),
    }

    let isolated_crate_name = format!("#{}", ISOLATED_MUTATION.fetch_add(1, SeqCst));
    let conf = TransformerConf {
        attr_id: 0,
        span: Span::call_site(),
        muttest_api: if std::env::var_os("CARGO_TARGET_TMPDIR").is_some() {
            quote!(::muttest_core::api) // integration test
        } else {
            quote!(crate::api) // unit test
        },
        crate_id: CrateId {
            pkg_name: "#isolated".into(),
            crate_name: isolated_crate_name.to_owned().into(),
        },
    };
    let mutables_conf = if !attr.is_empty() {
        let s = parse_macro_input!(attr as syn::LitStr);
        MutablesConf::One(s.value())
    } else {
        MutablesConf::All
    };

    // perform transformation
    let mut transformer = MuttestTransformer::new(conf, mutables_conf);
    let result = transformer.fold_item(input);

    let num_mutables = transformer.mut_count;
    let muttest_api = transformer.conf.muttest_api.clone();

    // construct definitions csv
    let mut definitions_csv = MUTABLE_DEFINITIONS_CSV_HEAD.to_owned().into_bytes();
    transformer
        .write_csv(csv::Writer::from_writer(&mut definitions_csv))
        .expect("unable to write definitions csv");
    let definitions_csv = String::from_utf8(definitions_csv).unwrap();

    // write context of transformation next to transformed function
    let mutable_definitions: &[syn::Item] = &[
        parse_quote!(pub const MUTABLES_CSV: &str = #definitions_csv;),
        parse_quote!(pub const NUM_MUTABLES: u32 = #num_mutables;),
        parse_quote!(pub const CRATE_ID: #muttest_api::CrateId = #muttest_api::CrateId {
            pkg_name: #muttest_api::Cow::Borrowed("#isolated"),
            crate_name: #muttest_api::Cow::Borrowed(#isolated_crate_name),
        };),
    ];

    match result {
        syn::Item::Fn(result) => {
            let mut mod_ident = result.sig.ident.clone();
            mod_ident.set_span(Span::call_site());
            quote! {
                #[allow(clippy::all)]
                #result
                mod #mod_ident {
                    #(#mutable_definitions)*
                }
            }
            .into()
        }
        syn::Item::Mod(mut result) => {
            result
                .content
                .as_mut()
                .unwrap()
                .1
                .extend(mutable_definitions.iter().cloned());
            result.to_token_stream().into()
        }
        _ => unreachable!(),
    }
}
