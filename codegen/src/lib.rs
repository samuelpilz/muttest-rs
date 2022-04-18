use std::{
    fs,
    io::Write,
    path::PathBuf,
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
    ExprLit, File, Item, Lit,
};

use muttest_core as core;

lazy_static! {
    static ref MUTATION_ID: AtomicUsize = AtomicUsize::new(1);
    static ref LOGGER: Mutex<fs::File> = {
        let dir = core::get_muttest_dir().expect("unable to get muttest directory");
        let crate_name = std::env::var("CARGO_PKG_NAME").expect("unable to get env var");
        let file_name = format!("transform-{crate_name}.log");

        fs::create_dir_all(&dir).expect("unable to create muttest directory");
        Mutex::new(fs::File::create(dir.join(file_name)).expect("unable to open logger file"))
    };
}

fn save_msg(msg: &str) {
    let mut f = LOGGER.lock().unwrap();
    writeln!(&mut f, "{}", msg).expect("unable to write log");
    f.flush().expect("unable to flush log");
}

#[proc_macro_attribute]
pub fn mutate(_attr: TokenStream, input: TokenStream) -> TokenStream {
    // TODO: optionally skip transformations based on ...?
    let input = parse_macro_input!(input as File);

    for i in &input.items {
        save_msg(&format!(
            "transform {} in {}",
            item_name(i),
            display_span(i.span())
        ));
    }

    let result = MuttestTransformer.fold_file(input);

    // TODO: optionally save before/after as logs

    result.into_token_stream().into()
}

fn item_name(item: &Item) -> String {
    match item {
        Item::Fn(i) => format!("fn {}", i.sig.ident),
        _ => todo!(),
    }
}

fn display_span(span: Span) -> String {
    let start = span.start();
    let end = span.end();
    format!(
        "{}@{}:{}-{}:{}",
        source_file_path(span).unwrap_or_default().display(),
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

/// reserves a new mutable id
fn new_mutable_id() -> usize {
    MUTATION_ID.fetch_add(1, Ordering::SeqCst)
}

struct MuttestTransformer;

impl Fold for MuttestTransformer {
    fn fold_expr(&mut self, e: Expr) -> Expr {
        let e = syn::fold::fold_expr(self, e);

        match e {
            Expr::Lit(ExprLit {
                lit: Lit::Int(i), ..
            }) => {
                // let suffix = i.suffix();
                let x: usize = i.base10_parse().unwrap();
                let mut_id = new_mutable_id();
                save_msg(&format!("MUTABLE {mut_id}: INT {x}"));
                parse_quote_spanned! {i.span()=>
                    ::muttest::mutable_int(#mut_id, module_path!(), #i)
                }
            }
            Expr::Binary(ExprBinary {
                ref left,
                op,
                ref right,
                ..
            }) if is_cmp_op(op) => {
                let op_str = op.to_token_stream().to_string();
                let mut_id = new_mutable_id();
                save_msg(&format!("MUTABLE {mut_id}: CMP {op_str}"));
                parse_quote_spanned! {op.span()=>
                    {
                        let (left, right) = (#left, #right);
                        // for type-inference, keep the original expression in the first branch
                        if false {left #op right} else {
                            ::muttest::mutable_cmp(#mut_id, module_path!(), #op_str, &left, &right)
                        }
                    }
                }
            }
            Expr::Binary(ExprBinary {
                ref left,
                op,
                ref right,
                ..
            }) if is_calc_op(op) => {
                let op_str = op.to_token_stream().to_string();
                let mut_id = new_mutable_id();
                save_msg(&format!("MUTABLE {mut_id}: CALC {op_str}"));

                let mutations = [
                    ("+", "add"),
                    ("-", "sub"),
                    ("*", "mul"),
                    ("/", "div"),
                    ("%", "rem"),
                ];
                let op_symbols = mutations.map(|x| x.0);
                let op_names = mutations.map(|x| Ident::new(x.1, op.span()));

                parse_quote_spanned! {op.span()=>
                    {
                        // arguments are evaluated before executing the calculation
                        let (left, right) = (#left, #right);
                        let left_type = ::muttest::phantom_for_type(&left);
                        let right_type = ::muttest::phantom_for_type(&right);
                        // this carries the output type of the computation
                        // the assignment in the default-case defines the type of this phantom
                        let mut output_type = ::core::marker::PhantomData;
                        #[allow(unused_assignments)]
                        match ::muttest::mutable_bin_op(#mut_id, module_path!(), #op_str) {
                            "" => {
                                let output = left #op right;
                                // after the computation is performed its output type is stored into the variable
                                // giving the compiler the necessary type hint required for the mutated cases
                                output_type = ::muttest::phantom_for_type(&output);
                                // report the possible mutations
                                ::muttest::report_possible_mutations(#mut_id,
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
                    }
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
