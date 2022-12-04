use std::{
    env::VarError,
    fs,
    io::Write,
    path::PathBuf,
    sync::{
        atomic::{AtomicUsize, Ordering::SeqCst},
        Mutex,
    },
};

use lazy_static::lazy_static;
use muttest_core::{
    context::ENV_VAR_MUTTEST_DIR,
    transformer::{
        MutablesConf, MuttestTransformer, TransformerConf, MUTABLE_DEFINITIONS_CSV_HEAD,
    },
    Error,
};
use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::{quote, ToTokens};
use syn::{fold::Fold, parse_macro_input, File};

lazy_static! {
    static ref MUTTEST_DIR: Option<PathBuf> = {
        match std::env::var(ENV_VAR_MUTTEST_DIR) {
            Ok(d) => Some(PathBuf::from(d)),
            Err(VarError::NotPresent) => None,
            Err(e) => panic!("{}", e),
        }
    };
    static ref PKG_NAME: String = std::env::var("CARGO_PKG_NAME").expect("unable to get env var");
    static ref CRATE_NAME: String =
        std::env::var("CARGO_CRATE_NAME").expect("unable to get env var");
    static ref MUTABLE_DEFINITIONS_FILE: Mutex<Option<fs::File>> =
        Mutex::new(open_definitions_file().expect("unable to open definitions file"));
}
static ATTR_ID: AtomicUsize = AtomicUsize::new(1);

fn open_definitions_file() -> Result<Option<fs::File>, Error> {
    match &*MUTTEST_DIR {
        Some(dir) => {
            let file_name = format!("mutable-definitions-{}:{}.csv", &*PKG_NAME, &*CRATE_NAME);
            let mut file = fs::File::create(PathBuf::from(dir).join(file_name))?;
            write!(&mut file, "{MUTABLE_DEFINITIONS_CSV_HEAD}")?;
            file.flush()?;
            file.sync_all()?;
            Ok(Some(file))
        }
        _ => Ok(None),
    }
}

/// Macro to enable mutation testing.
#[proc_macro_attribute]
pub fn mutate(_attr: TokenStream, input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as File);

    let conf = TransformerConf {
        attr_id: ATTR_ID.fetch_add(1, SeqCst),
        span: Span::call_site(),
        mutables: MutablesConf::All,
        muttest_api: quote!(muttest),
        pkg_name: &PKG_NAME,
        crate_name: CRATE_NAME.clone(),
    };

    let mut transformer = MuttestTransformer::new(conf);
    let result = transformer.fold_file(input);

    if let Some(f) = &mut *MUTABLE_DEFINITIONS_FILE.lock().unwrap() {
        for l in transformer.definitions() {
            writeln!(f, "{l}").expect("unable to write to definitions file");
        }
    }

    result.into_token_stream().into()
}

/// isolated mutation for unit testing of `muttest-core` itself.
#[cfg(feature = "selftest")]
#[proc_macro_attribute]
pub fn mutate_isolated(attr: TokenStream, input: TokenStream) -> TokenStream {
    static ISOLATED_MUTATION: AtomicUsize = AtomicUsize::new(1);

    // TODO: hide behind feature (unnecessary codegen)
    let input = parse_macro_input!(input as syn::ItemFn);

    let pkg_name = std::env::var("CARGO_PKG_NAME").unwrap();
    if pkg_name != "muttest-core" && pkg_name != "muttest-selftest" {
        // TODO: compiler error instead of panic
        panic!("`mutate_isolated` should only be used for internal testing");
    }

    let crate_name = format!("#{}", ISOLATED_MUTATION.fetch_add(1, SeqCst));
    let mut conf = TransformerConf {
        attr_id: 0,
        span: Span::call_site(),
        mutables: MutablesConf::All,
        muttest_api: if std::env::var_os("CARGO_TARGET_TMPDIR").is_some() {
            quote!(::muttest_core::api) // integration test
        } else {
            quote!(crate::api) // unit test
        },
        pkg_name: "#isolated",
        crate_name: crate_name.to_owned(),
    };
    if !attr.is_empty() {
        let s = parse_macro_input!(attr as syn::LitStr);
        conf.mutables = MutablesConf::One(s.value());
    }

    // perform transformation
    let mut transformer = MuttestTransformer::new(conf);
    let result = transformer.fold_item_fn(input);

    let mut definitions_csv = MUTABLE_DEFINITIONS_CSV_HEAD.to_owned();
    for l in transformer.definitions() {
        definitions_csv.push_str(l);
        definitions_csv.push('\n');
    }
    let num_mutables = transformer.num_mutables();

    // write context of transformation next to transformed function
    let mut mod_ident = result.sig.ident.clone();
    mod_ident.set_span(Span::call_site());
    quote! {
        #[allow(clippy::all)]
        #result
        mod #mod_ident {
            pub const MUTABLES_CSV: &str = #definitions_csv;
            pub const NUM_MUTABLES: usize = #num_mutables;
            pub const PKG_NAME: &str = "#isolated";
            pub const CRATE_NAME: &str = #crate_name;
        }
    }
    .into()
}
