use std::{
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
use proc_macro::TokenStream;
use proc_macro2::{Ident, Span};
use quote::ToTokens;
use syn::{
    fold::Fold, parse_macro_input, parse_quote_spanned, spanned::Spanned, BinOp, Expr, ExprBinary,
    ExprLit, File, Lit,
};

lazy_static! {
    static ref MUTABLE_ID: AtomicUsize = AtomicUsize::new(1);
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

// TODO: use MuttestError instead
fn open_definitions_file() -> Result<Option<fs::File>, std::io::Error> {
    match option_env!("MUTTEST_DIR") {
        None => Ok(None),
        Some(dir) => {
            let dir = PathBuf::from(dir);
            fs::create_dir_all(&dir)?;
            let file_name = format!("definitions-{}.csv", &*TARGET_NAME);
            let mut file = fs::File::create(dir.join(file_name))?;
            writeln!(&mut file, "id,kind,code,loc")?;
            file.flush()?;
            file.sync_all()?;
            Ok(Some(file))
        }
    }
}

fn save_mutable(m_id: usize, mut_kind: &str, code: &str, loc: &str) {
    let mut f = MUTABLE_DEFINITIONS_FILE.lock().unwrap();
    if let Some(f) = f.deref_mut() {
        writeln!(f, "{m_id},{mut_kind},{},{loc}", code).expect("unable to write mutable file");
        f.flush().expect("unable to flush mutable file");
    }
}

#[proc_macro_attribute]
pub fn mutate(_attr: TokenStream, input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as File);

    let result = MuttestTransformer.fold_file(input);

    result.into_token_stream().into()
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

/// reserves and returns a new mutable id
fn new_mutable_id() -> usize {
    MUTABLE_ID.fetch_add(1, Ordering::SeqCst)
}

struct MuttestTransformer;

impl Fold for MuttestTransformer {
    // TODO: skip consts & tests
    fn fold_expr(&mut self, e: Expr) -> Expr {
        let e = syn::fold::fold_expr(self, e);

        match e {
            Expr::Lit(ExprLit {
                lit: Lit::Int(i), ..
            }) => {
                let m_id = new_mutable_id();
                save_mutable(m_id, "int", i.base10_digits(), &display_span(i.span()));
                let crate_name = &*TARGET_NAME;
                let m_id: Expr = parse_quote_spanned! {i.span() =>
                    ::muttest::MutableId {id: #m_id, crate_name: #crate_name}
                };
                parse_quote_spanned! {i.span()=>
                    ({
                        ::muttest::report_location(#m_id, file!(), line!(), column!());
                        ::muttest::mutable_int(#m_id, #i)
                    },).0
                }
            }
            Expr::Binary(ExprBinary {
                left, op, right, ..
            }) if is_cmp_op(op) => {
                let op_str = op.to_token_stream().to_string();
                let m_id = new_mutable_id();
                save_mutable(m_id, "cmp", &op_str, &display_span(op.span()));
                let crate_name = &*TARGET_NAME;
                let m_id: Expr = parse_quote_spanned! {op.span() =>
                    ::muttest::MutableId {id: #m_id, crate_name: #crate_name}
                };
                parse_quote_spanned! {op.span()=>
                    ({
                        ::muttest::report_location(#m_id, file!(), line!(), column!());
                        let (left, right) = (#left, #right);
                        // for type-inference, keep the original expression in the first branch
                        if false {left #op right} else {
                            ::muttest::mutable_cmp(#m_id, #op_str, &left, &right)
                        }
                    },).0
                }
            }
            Expr::Binary(ExprBinary {
                left, op, right, ..
            }) if is_calc_op(op) => {
                let op_str = op.to_token_stream().to_string();
                let m_id = new_mutable_id();
                save_mutable(m_id, "calc", &op_str, &display_span(op.span()));

                let mutations = [
                    ("+", "add"),
                    ("-", "sub"),
                    ("*", "mul"),
                    ("/", "div"),
                    ("%", "rem"),
                ];
                let op_symbols = mutations.map(|x| x.0);
                let op_names = mutations.map(|x| Ident::new(x.1, op.span()));

                let crate_name = &*TARGET_NAME;
                let m_id: Expr = parse_quote_spanned! {op.span() =>
                    ::muttest::MutableId {id: #m_id, crate_name: #crate_name}
                };
                parse_quote_spanned! {op.span()=>
                    ({
                        ::muttest::report_location(#m_id, file!(), line!(), column!());
                        // arguments are evaluated before executing the calculation
                        let (left, right) = (#left, #right);
                        let left_type = ::muttest::phantom_for_type(&left);
                        let right_type = ::muttest::phantom_for_type(&right);
                        // this carries the output type of the computation
                        // the assignment in the default-case defines the type of this phantom
                        let mut output_type = ::core::marker::PhantomData;
                        let mut_op = ::muttest::mutable_bin_op(#m_id, #op_str);
                        #[allow(unused_assignments)]
                        match mut_op {
                            "" => {
                                let output = left #op right;
                                // after the computation is performed its output type is stored into the variable
                                // giving the compiler the necessary type hint required for the mutated cases
                                output_type = ::muttest::phantom_for_type(&output);
                                // report the possible mutations
                                ::muttest::report_possible_mutations(#m_id,
                                    &[
                                        #((
                                            #op_symbols,
                                            ::muttest::get_binop!(#op_names, left_type, right_type, output_type)
                                                .is_impl()
                                        ),)*
                                    ]
                                );
                                output
                            },
                            // possible mutations
                            #(#op_symbols =>
                                ::muttest::get_binop!(#op_names, left_type, right_type, output_type).run(left, right),
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
