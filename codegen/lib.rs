use std::{
    ops::{ControlFlow, Deref, DerefMut},
};

use muttest_core::{
    mutable::{binop_cmp::MutableBinopCmp, lit_int::MutableLitInt},
    transformer::*,
};
use proc_macro::TokenStream;
use proc_macro2::{Ident, Span};
use quote::ToTokens;
use syn::{
    fold::Fold, parse_macro_input, parse_quote, parse_quote_spanned, spanned::Spanned, BinOp, Expr,
    ExprBinary, ExprLit, File, Item, ItemFn, Lit, LitStr,
};

/// isolated mutation for testing purposes
#[proc_macro_attribute]
pub fn mutate_isolated(attr: TokenStream, input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as ItemFn);

    let mut transformer = MuttestTransformer::new_isolated();
    if !attr.is_empty() {
        let s = parse_macro_input!(attr as LitStr);
        transformer.conf.mutables = MutablesConf::One(s.value())
    }
    let result = FoldImpl(&mut transformer).fold_item_fn(input);

    // write muttest "logs"
    let mut mod_ident = result.sig.ident.clone();
    mod_ident.set_span(Span::call_site());
    let mutables_csv = &transformer.isolated.as_ref().unwrap().mutables_csv;
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
    let result = FoldImpl(&mut transformer).fold_file(input);

    result.into_token_stream().into()
}

#[proc_macro_attribute]
pub fn mutate(_attr: TokenStream, input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as File);

    let mut transformer = MuttestTransformer::new();
    let result = FoldImpl(&mut transformer).fold_file(input);

    result.into_token_stream().into()
}

struct FoldImpl<'a>(&'a mut MuttestTransformer);
impl Deref for FoldImpl<'_> {
    type Target = MuttestTransformer;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl DerefMut for FoldImpl<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

trait MatchMutable<'a>: Sized + Mutable<'a> {
    fn match_expr<'b: 'a>(expr: &'b Expr) -> Option<Self>;
}

impl<'a> MatchMutable<'a> for MutableLitInt<'a> {
    fn match_expr<'b: 'a>(expr: &'b Expr) -> Option<Self> {
        match expr {
            Expr::Lit(ExprLit {
                lit: Lit::Int(i), ..
            }) => Some(MutableLitInt {
                base10_digits: i.base10_digits(),
                span: i.span(),
                tokens: i,
            }),
            _ => None,
        }
    }
}
impl<'a> MatchMutable<'a> for MutableBinopCmp<'a> {
    fn match_expr<'b: 'a>(expr: &'b Expr) -> Option<Self> {
        match expr {
            Expr::Binary(ExprBinary {
                left, op, right, ..
            }) if is_cmp_op(*op) => Some(Self {
                left,
                right,
                op,
                span: op.span(),
            }),
            _ => None,
        }
    }
}

impl FoldImpl<'_> {
    fn try_mutate_expr<'a, 'b: 'a, M: MatchMutable<'a>>(
        &mut self,
        mutable_name: &str,
        expr: &'b Expr,
    ) -> ControlFlow<Expr> {
        if self.should_mutate(mutable_name) {
            if let Some(m) = M::match_expr(expr) {
                return ControlFlow::Break(
                    syn::parse2(m.transform(self)).expect("transform syntax error"),
                );
            }
        }
        ControlFlow::Continue(())
    }

    fn try_all_mutate_expr(&mut self, e: &Expr) -> ControlFlow<Expr> {
        self.try_mutate_expr::<MutableLitInt>("lit_int", &e)?;
        self.try_mutate_expr::<MutableBinopCmp>("binop_cmp", &e)?;

        ControlFlow::Continue(())
    }
}

impl Fold for FoldImpl<'_> {
    // TODO: skip consts & tests ...
    fn fold_item(&mut self, i: Item) -> Item {
        match i {
            Item::Const(_) => i,
            _ => syn::fold::fold_item(self, i),
        }
    }

    fn fold_expr(&mut self, e: Expr) -> Expr {
        let e = syn::fold::fold_expr(self, e);

        if let ControlFlow::Break(e) = self.try_all_mutate_expr(&e) {
            return e;
        }

        match e {
            // TODO: also byteStr
            Expr::Lit(ExprLit {
                lit: Lit::Str(s), ..
            }) if self.should_mutate("lit_str") => {
                let m_id = self.register_new_mutable(
                    "str",
                    &format!("{:?}", s.value()),
                    &display_span(s.span()),
                );
                let m_id = self.mutable_id_expr(&m_id, s.span());
                let core_crate = self.core_crate_path(s.span());
                parse_quote_spanned! {s.span()=>
                    ({
                        #core_crate::report_location(&#m_id, file!(), line!(), column!());
                        static MUTABLE: ::std::sync::RwLock<Option<&'static str>> = ::std::sync::RwLock::new(::std::option::Option::None);
                        #core_crate::mutable::lit_str::mutable_str(&#m_id, #s, &MUTABLE)
                    },).0
                }
            }
            Expr::Binary(ExprBinary {
                left, op, right, ..
            }) if is_bool_op(op) && self.should_mutate("binop_bool") => {
                let op_str = op.to_token_stream().to_string();
                let m_id = self.register_new_mutable("bool", &op_str, &display_span(op.span()));
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
            }) if is_calc_op(op) && self.should_mutate("binop_calc") => {
                let op_str = op.to_token_stream().to_string();
                let m_id = self.register_new_mutable("calc", &op_str, &display_span(op.span()));

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
                        let mut_op = #core_crate::mutable::binop_calc::mutable_binop_calc(&#m_id, #op_str);
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
                                            {
                                                #[allow(unused_imports)]
                                                use #core_crate::mutable::binop_calc::#op_names::{IsNo, IsYes};
                                                (&(left_type, right_type, output_type))
                                                    .get_impl()
                                                    .is_impl()
                                            }
                                        ),)*
                                    ]
                                );
                                output
                            },
                            // possible mutations
                            #(#op_symbols =>
                                {
                                    #[allow(unused_imports)]
                                    use #core_crate::mutable::binop_calc::#op_names::{IsNo, IsYes};
                                    (&(left_type, right_type, output_type))
                                        .get_impl()
                                        .run(left, right)
                                }
                            )*
                            // TODO: report error and panic
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
