use std::{
    borrow::Borrow,
    fs,
    io::Write,
    ops::DerefMut,
    path::{Path, PathBuf},
    sync::{
        atomic::{AtomicUsize, Ordering},
        Mutex,
    },
};

use lazy_static::lazy_static;
use muttest_core::{Error, MutableId, MUTTEST_DIR};
use proc_macro::TokenStream;
use proc_macro2::{Ident, Span};
use quote::ToTokens;
use syn::{
    fold::Fold, parse_macro_input, parse_quote, parse_quote_spanned, spanned::Spanned, BinOp, Expr,
    ExprBinary, ExprLit, File, Item, ItemFn, Lit,
};

/// isolated mutation for testing purposes
#[proc_macro_attribute]
pub fn mutate_isolated(_attr: TokenStream, input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as ItemFn);

    let mut transformer = MuttestTransformer::new_isolated();

    let result = transformer.fold_item_fn(input);

    // write muttest "logs"
    let mut mod_ident = result.sig.ident.clone();
    mod_ident.set_span(Span::call_site());
    let mutables_csv = &transformer.isolated.as_ref().unwrap().mutables;
    let num_mutables = transformer.isolated.as_ref().unwrap().local_id;

    let result: File = parse_quote! {
        #result
        mod #mod_ident {
            pub const MUTABLES_CSV: &str = #mutables_csv;
            pub const NUM_MUTABLES: usize = #num_mutables;
        }
    };

    result.into_token_stream().into()
}

#[proc_macro_attribute]
pub fn mutate_selftest(_attr: TokenStream, input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as File);

    let mut transformer = MuttestTransformer::new_selftest();

    let result = transformer.fold_file(input);

    result.into_token_stream().into()

}

#[proc_macro_attribute]
pub fn mutate(_attr: TokenStream, input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as File);

    let mut transformer = MuttestTransformer::new();

    let result = transformer.fold_file(input);

    result.into_token_stream().into()
}

lazy_static! {
    static ref MUTABLE_ID_NUM: AtomicUsize = AtomicUsize::new(1);
    static ref TARGET_NAME: String = {
        let mut target_name = std::env::var("CARGO_PKG_NAME").expect("unable to get env var");
        let crate_name = std::env::var("CARGO_CRATE_NAME").expect("unable to get env var");
        if crate_name != target_name {
            target_name.push_str(":");
            target_name.push_str(&crate_name);
        }
        target_name
    };
    static ref MUTABLE_DEFINITIONS_FILE: Mutex<Option<fs::File>> =
        Mutex::new(open_definitions_file().expect("unable to open definitions file"));
}

const MUTABLE_DEFINITIONS_CSV_HEAD: &str = "id,kind,code,loc";

// TODO: use MuttestError instead
fn open_definitions_file() -> Result<Option<fs::File>, Error> {
    match &*MUTTEST_DIR {
        Some(dir) => {
            let file_name = format!("mutable-definitions-{}.csv", &*TARGET_NAME);
            let mut file = fs::File::create(PathBuf::from(dir).join(file_name))?;
            writeln!(&mut file, "id,kind,code,loc")?;
            file.flush()?;
            file.sync_all()?;
            Ok(Some(file))
        }
        _ => Ok(None),
    }
}

fn display_span(span: Span) -> String {
    let start = span.start();
    let end = span.end();
    format!(
        "{}@{}:{}-{}:{}",
        source_file_path(span)
            .as_deref()
            .unwrap_or_else(|| Path::new("<unknown-file>"))
            .display(),
        start.line,
        start.column,
        end.line,
        end.column
    )
}

#[cfg(procmacro2_semver_exempt)]
fn source_file_path(span: Span) -> Option<PathBuf> {
    Some(span.source_file().path())
}
#[cfg(not(procmacro2_semver_exempt))]
#[allow(unused_variables)]
fn source_file_path(span: Span) -> Option<PathBuf> {
    None
}

struct MuttestTransformer {
    core_crate: Option<&'static str>,
    isolated: Option<MutateData>,
}
struct MutateData {
    local_id: usize,
    mutables: String,
}
impl MuttestTransformer {
    fn new() -> Self {
        Self {
            core_crate: Some("muttest"),
            isolated: None,
        }
    }
    fn new_isolated() -> Self {
        Self {
            core_crate: None,
            isolated: Some(MutateData {
                local_id: 0,
                mutables: format!("{MUTABLE_DEFINITIONS_CSV_HEAD}\n"),
            }),
        }
    }
    fn new_selftest() -> Self {
        Self {
            core_crate: None,
            isolated: None,
        }
    }

    /// register a new mutable
    fn register_mutable(&mut self, mut_kind: &str, code: &str, loc: &str) -> MutableId<'static> {
        match &mut self.isolated {
            Some(data) => {
                data.local_id += 1;
                let id = data.local_id;

                data.mutables += &format!("{id},{mut_kind},{},{loc}\n", code);

                MutableId {
                    id,
                    crate_name: ::std::borrow::Cow::Borrowed(""),
                }
            }
            None => {
                let id = MUTABLE_ID_NUM.fetch_add(1, Ordering::SeqCst);

                let mut f = MUTABLE_DEFINITIONS_FILE.lock().unwrap();
                if let Some(f) = f.deref_mut() {
                    writeln!(f, "{id},{mut_kind},{},{loc}", code)
                        .expect("unable to write mutable file");
                    f.flush().expect("unable to flush mutable file");
                }

                MutableId {
                    id,
                    crate_name: ::std::borrow::Cow::Borrowed(&*TARGET_NAME),
                }
            }
        }
    }

    fn mutable_id_expr(&self, m_id: &MutableId, span: Span) -> Expr {
        let id = m_id.id;
        let crate_name: &str = m_id.crate_name.borrow();
        let core_crate = self.core_crate_path(span);

        parse_quote_spanned! {span =>
            #core_crate::MutableId {id: #id, crate_name: ::std::borrow::Cow::Borrowed(#crate_name)}
        }
    }

    // TODO: maybe this can be done with hygiene instead?
    fn core_crate_path(&self, span: Span) -> syn::Path {
        match self.core_crate {
            Some(cc) => {
                let cc = Ident::new(cc, span);
                parse_quote_spanned! {span => ::#cc}
            }
            None => parse_quote_spanned! {span => crate},
        }
    }
}

impl Fold for MuttestTransformer {
    // TODO: skip consts & tests ...
    fn fold_item(&mut self, i: Item) -> Item {
        match i {
            Item::Const(_) => i,
            _ => syn::fold::fold_item(self, i),
        }
    }

    fn fold_expr(&mut self, e: Expr) -> Expr {
        let e = syn::fold::fold_expr(self, e);

        match e {
            Expr::Lit(ExprLit {
                lit: Lit::Int(i), ..
            }) => {
                let m_id = self.register_mutable("int", i.base10_digits(), &display_span(i.span()));

                let m_id = self.mutable_id_expr(&m_id, i.span());
                let core_crate = self.core_crate_path(i.span());
                parse_quote_spanned! {i.span()=>
                    ({
                        #core_crate::report_location(&#m_id, file!(), line!(), column!());
                        #core_crate::mutable_int(&#m_id, #i)
                    },).0
                }
            }
            Expr::Lit(ExprLit {
                lit: Lit::Str(s), ..
            }) => {
                let m_id = self.register_mutable(
                    "str",
                    &format!("{:?}", s.value()),
                    &display_span(s.span()),
                );
                let m_id = self.mutable_id_expr(&m_id, s.span());
                let core_crate = self.core_crate_path(s.span());
                parse_quote_spanned! {s.span()=>
                    ({
                        #core_crate::report_location(&#m_id, file!(), line!(), column!());
                        static MUTATION: ::std::sync::RwLock<Option<&'static str>> = ::std::sync::RwLock::new(::std::option::Option::None);
                        #core_crate::mutable_str(&#m_id, #s, &MUTATION)
                    },).0
                }
            }
            // TODO: also byteStr
            Expr::Binary(ExprBinary {
                left, op, right, ..
            }) if is_bool_op(op) => {
                let op_str = op.to_token_stream().to_string();
                let m_id = self.register_mutable("bool", &op_str, &display_span(op.span()));
                let m_id = self.mutable_id_expr(&m_id, op.span());
                let core_crate = self.core_crate_path(op.span());
                parse_quote_spanned! {op.span()=>
                    ({
                        #core_crate::report_location(&#m_id, file!(), line!(), column!());
                        // for type-inference, keep the original expression in the first branch
                        if false {left #op right} else {
                            #left #op #right
                        }
                    },).0
                }
            }
            Expr::Binary(ExprBinary {
                left, op, right, ..
            }) if is_cmp_op(op) => {
                let op_str = op.to_token_stream().to_string();
                let m_id = self.register_mutable("cmp", &op_str, &display_span(op.span()));
                let m_id = self.mutable_id_expr(&m_id, op.span());
                let core_crate = self.core_crate_path(op.span());
                parse_quote_spanned! {op.span()=>
                    ({
                        #core_crate::report_location(&#m_id, file!(), line!(), column!());
                        let (left, right) = (#left, #right);
                        // for type-inference, keep the original expression in the first branch
                        if false {left #op right} else {
                            #core_crate::mutable_cmp(&#m_id, #op_str, &left, &right)
                        }
                    },).0
                }
            }
            Expr::Binary(ExprBinary {
                left, op, right, ..
            }) if is_calc_op(op) => {
                let op_str = op.to_token_stream().to_string();
                let m_id = self.register_mutable("calc", &op_str, &display_span(op.span()));

                let mutations = [
                    ("+", "add"),
                    ("-", "sub"),
                    ("*", "mul"),
                    ("/", "div"),
                    ("%", "rem"),
                ];
                let op_symbols = mutations.map(|x| x.0);
                let op_names = mutations.map(|x| Ident::new(x.1, op.span()));

                let m_id = self.mutable_id_expr(&m_id, op.span());
                let core_crate = self.core_crate_path(op.span());
                parse_quote_spanned! {op.span()=>
                    ({
                        #core_crate::report_location(&#m_id, file!(), line!(), column!());
                        // arguments are evaluated before executing the calculation
                        let (left, right) = (#left, #right);
                        let left_type = #core_crate::phantom_for_type(&left);
                        let right_type = #core_crate::phantom_for_type(&right);
                        // this carries the output type of the computation
                        // the assignment in the default-case defines the type of this phantom
                        let mut output_type = ::core::marker::PhantomData;
                        let mut_op = #core_crate::mutable_bin_op(&#m_id, #op_str);
                        #[allow(unused_assignments)]
                        match mut_op {
                            "" => {
                                let output = left #op right;
                                // after the computation is performed its output type is stored into the variable
                                // giving the compiler the necessary type hint required for the mutated cases
                                output_type = #core_crate::phantom_for_type(&output);
                                // report the possible mutations
                                #core_crate::report_possible_mutations(&#m_id,
                                    &[
                                        #((
                                            #op_symbols,
                                            #core_crate::get_binop!(#core_crate::#op_names, left_type, right_type, output_type)
                                                .is_impl()
                                        ),)*
                                    ]
                                );
                                output
                            },
                            // possible mutations
                            #(#op_symbols =>
                                #core_crate::get_binop!(#core_crate::#op_names, left_type, right_type, output_type).run(left, right),
                            )*
                            // the base case needs to be first in order to give the compiler the correct type hints
                            _ => unreachable!(),
                        }
                    },).0
                }
            }
            _ => e,
        }
    }
}

fn is_bool_op(op: BinOp) -> bool {
    match op {
        BinOp::And(_) => true,
        BinOp::Or(_) => true,
        _ => false,
    }
}

fn is_calc_op(op: BinOp) -> bool {
    match op {
        BinOp::Add(_) => true,
        BinOp::Sub(_) => true,
        BinOp::Mul(_) => true,
        BinOp::Div(_) => true,
        BinOp::Rem(_) => true,
        _ => false,
    }
}

fn is_cmp_op(op: BinOp) -> bool {
    match op {
        BinOp::Lt(_) => true,
        BinOp::Le(_) => true,
        BinOp::Ge(_) => true,
        BinOp::Gt(_) => true,
        _ => false,
    }
}
