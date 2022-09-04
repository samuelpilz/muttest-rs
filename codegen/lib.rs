use std::ops::{ControlFlow, Deref, DerefMut};

use muttest_core::{
    mutable::{
        binop_bool::MutableBinopBool, binop_calc::MutableBinopCalc, binop_cmp::MutableBinopCmp,
        extreme::MutableExtreme, lit_int::MutableLitInt, lit_str::MutableLitStr, binop_eq::MutableBinopEq,
    },
    transformer::*,
};
use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::ToTokens;
use syn::{
    fold::Fold, parse_macro_input, parse_quote, spanned::Spanned, BinOp, Expr, ExprBinary, ExprLit,
    File, ItemConst, ItemFn, Lit, LitStr,
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
    // TODO: maybe only transform if env-vars set
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

/// trait to extract a mutable from AST nodes
trait MatchMutable<'m, T: syn::parse::Parse>: Sized + Mutable<'m> {
    fn try_match<'a: 'm>(ast_node: &'a T) -> Option<Self>;
}

impl<'a> MatchMutable<'a, Expr> for MutableLitInt<'a> {
    fn try_match<'b: 'a>(expr: &'b Expr) -> Option<Self> {
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
impl<'a> MatchMutable<'a, Expr> for MutableLitStr<'a> {
    fn try_match<'b: 'a>(expr: &'b Expr) -> Option<Self> {
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
impl<'a> MatchMutable<'a, Expr> for MutableBinopEq<'a> {
    fn try_match<'b: 'a>(expr: &'b Expr) -> Option<Self> {
        match expr {
            Expr::Binary(ExprBinary {
                left, op, right, ..
            }) if is_eq_op(*op) => Some(Self {
                left,
                right,
                op,
                span: op.span(),
            }),
            _ => None,
        }
    }
}
impl<'a> MatchMutable<'a, Expr> for MutableBinopCmp<'a> {
    fn try_match<'b: 'a>(expr: &'b Expr) -> Option<Self> {
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
impl<'a> MatchMutable<'a, Expr> for MutableBinopCalc<'a> {
    fn try_match<'b: 'a>(expr: &'b Expr) -> Option<Self> {
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
impl<'a> MatchMutable<'a, Expr> for MutableBinopBool<'a> {
    fn try_match<'b: 'a>(expr: &'b Expr) -> Option<Self> {
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
impl<'a> MatchMutable<'a, ItemFn> for MutableExtreme<'a> {
    fn try_match<'b: 'a>(item_fn: &'b ItemFn) -> Option<Self> {
        let ItemFn {
            vis, sig, block, ..
        } = item_fn;
        Some(Self {
            vis,
            sig,
            block,
            span: item_fn.span(), // TODO: smaller span instead?
        })
    }
}

impl FoldImpl<'_> {
    fn try_mutate<'a, 'b: 'a, T: syn::parse::Parse, M: MatchMutable<'a, T>>(
        &mut self,
        expr: &'b T,
    ) -> ControlFlow<T> {
        if !self.should_mutate(M::NAME) {
            return ControlFlow::Continue(());
        }
        if let Some(m) = M::try_match(expr) {
            return ControlFlow::Break(match syn::parse2(m.transform(self)) {
                Ok(m) => m,
                Err(e) => panic!("mutable `{}` transform syntax error: {}", M::NAME, e),
            });
        }
        ControlFlow::Continue(())
    }

    fn try_all_mutate_expr(&mut self, e: &Expr) -> ControlFlow<Expr> {
        self.try_mutate::<Expr, MutableLitInt>(&e)?;
        // TODO: also byteStr
        self.try_mutate::<Expr, MutableLitStr>(&e)?;
        self.try_mutate::<Expr, MutableBinopEq>(&e)?;
        self.try_mutate::<Expr, MutableBinopCmp>(&e)?;
        self.try_mutate::<Expr, MutableBinopCalc>(&e)?;
        self.try_mutate::<Expr, MutableBinopBool>(&e)?;

        ControlFlow::Continue(())
    }

    fn try_all_mutate_item_fn(&mut self, item_fn: &ItemFn) -> ControlFlow<ItemFn> {
        self.try_mutate::<ItemFn, MutableExtreme>(&item_fn)?;

        ControlFlow::Continue(())
    }
}

impl Fold for FoldImpl<'_> {
    // TODO: inspect & preserve attrs
    // TODO: skip consts & tests ...
    fn fold_item_const(&mut self, item_const: ItemConst) -> ItemConst {
        item_const
    }

    fn fold_item_fn(&mut self, f: ItemFn) -> ItemFn {
        if f.sig.constness.is_some() {
            return f;
        }

        let f = syn::fold::fold_item_fn(self, f);

        match self.try_all_mutate_item_fn(&f) {
            ControlFlow::Break(f) => f,
            ControlFlow::Continue(_) => f,
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

fn is_eq_op(op: BinOp) -> bool {
    match op {
        BinOp::Eq(_) => true,
        BinOp::Ne(_) => true,
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

fn is_calc_op(op: BinOp) -> bool {
    match op {
        BinOp::Add(_) => true,
        BinOp::Sub(_) => true,
        BinOp::Mul(_) => true,
        BinOp::Div(_) => true,
        BinOp::Rem(_) => true,
        BinOp::BitOr(_) => true,
        BinOp::BitAnd(_) => true,
        BinOp::BitXor(_) => true,
        BinOp::Shl(_) => true,
        BinOp::Shr(_) => true,
        _ => false,
    }
}
