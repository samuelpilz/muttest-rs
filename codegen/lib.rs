use std::ops::{ControlFlow, Deref, DerefMut};

use muttest_core::{
    mutable::{
        binop_calc::MutableBinopCalc, binop_cmp::MutableBinopCmp, lit_int::MutableLitInt,
        lit_str::MutableLitStr, binop_bool::MutableBinopBool,
    },
    transformer::*,
};
use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::ToTokens;
use syn::{
    fold::Fold, parse_macro_input, parse_quote, spanned::Spanned, BinOp, Expr,
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
                lit: Lit::Int(l), ..
            }) => Some(Self {
                base10_digits: l.base10_digits(),
                span: l.span(),
                lit: l,
            }),
            _ => None,
        }
    }
}
impl<'a> MatchMutable<'a> for MutableLitStr<'a> {
    fn match_expr<'b: 'a>(expr: &'b Expr) -> Option<Self> {
        match expr {
            Expr::Lit(ExprLit {
                lit: Lit::Str(l), ..
            }) => Some(Self {
                value: l.value(),
                span: l.span(),
                lit: l,
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
impl<'a> MatchMutable<'a> for MutableBinopCalc<'a> {
    fn match_expr<'b: 'a>(expr: &'b Expr) -> Option<Self> {
        match expr {
            Expr::Binary(ExprBinary {
                left, op, right, ..
            }) if is_calc_op(*op) => Some(Self {
                left,
                right,
                op,
                span: op.span(),
            }),
            _ => None,
        }
    }
}
impl<'a> MatchMutable<'a> for MutableBinopBool<'a> {
    fn match_expr<'b: 'a>(expr: &'b Expr) -> Option<Self> {
        match expr {
            Expr::Binary(ExprBinary {
                left, op, right, ..
            }) if is_bool_op(*op) => Some(Self {
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
        // TODO: also byteStr
        self.try_mutate_expr::<MutableLitStr>("lit_str", &e)?;
        self.try_mutate_expr::<MutableBinopCmp>("binop_cmp", &e)?;
        self.try_mutate_expr::<MutableBinopCalc>("binop_calc", &e)?;
        self.try_mutate_expr::<MutableBinopBool>("binop_bool", &e)?;

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

        match self.try_all_mutate_expr(&e) {
            ControlFlow::Break(e) => e,
            ControlFlow::Continue(_) => e,
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
