use std::sync::atomic::{AtomicUsize, Ordering};

use lazy_static::lazy_static;
use proc_macro::TokenStream;
use quote::{quote_spanned, ToTokens};
use syn::{
    fold::Fold, parse_macro_input, parse_quote_spanned, spanned::Spanned, BinOp, Expr, ExprBinary,
    ExprLit, Item, Lit,
};

#[proc_macro_attribute]
pub fn mutate(_attr: TokenStream, input: TokenStream) -> TokenStream {
    // TODO: optionally skip transformations based on ...?
    let input = parse_macro_input!(input as Item);

    let result = MuttestTransformer.fold_item(input);

    // TODO: optionally save before/after as logs

    result.into_token_stream().into()
}

lazy_static! {
    static ref MUTATION_ID: AtomicUsize = AtomicUsize::new(1);
}

/// reserves mutation ids, the first is returned
fn get_mutation_ids(num_mutations: usize) -> usize {
    MUTATION_ID.fetch_add(num_mutations, Ordering::SeqCst)
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
                let mutations = [x + 1, x - 1];
                let mut_id = get_mutation_ids(mutations.len());
                let ids = 0usize..;
                parse_quote_spanned! {i.span()=>
                    (match ::muttest::get_mutation_for(#mut_id) {
                        #(#ids => #mutations,)*
                        _ => #x,
                    })
                }
            }
            Expr::Binary(ExprBinary {
                ref left,
                op,
                ref right,
                ..
            }) => {
                let span = op.span();
                let mutations = match op {
                    BinOp::Add(_) => [quote_spanned! {span=> -}],
                    _ => return e,
                };
                let mut_id = get_mutation_ids(mutations.len());
                let ids = 0usize..;
                parse_quote_spanned! {op.span()=>
                    {
                        let (left, right) = (#left, #right);
                        match ::muttest::get_mutation_for(#mut_id) {
                            #(#ids => left #mutations right,)*
                            _ => left #op right,
                    }}
                }
            }
            _ => e,
        }
    }
}
