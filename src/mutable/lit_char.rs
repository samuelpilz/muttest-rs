use proc_macro2::{Span, TokenStream};
use quote::{quote_spanned, ToTokens};
use syn::{Expr, ExprLit, Lit};

use crate::{
    transformer::{MuttestTransformer, TransformSnippets},
    BakedLocation, BakedMutableId,
};

use super::{FilterMutableCode, MatchMutable};

pub struct Mutable<'a> {
    pub c: char,
    pub span: Span,
    pub lit: &'a dyn ToTokens,
}

impl<'a> MatchMutable<'a, Expr> for Mutable<'a> {
    fn try_match(expr: &'a Expr) -> Option<Self> {
        match expr {
            Expr::Lit(ExprLit {
                lit: Lit::Char(l), ..
            }) => Some(Self {
                c: l.value(),
                span: l.span(),
                lit: l,
            }),
            _ => None,
        }
    }
}

impl<'a> super::Mutable<'a> for Mutable<'a> {
    const NAME: &'static str = "lit_char";

    fn span(&self) -> Span {
        self.span
    }

    fn transform(self, transformer: &mut MuttestTransformer) -> TokenStream {
        let span = self.span;
        let lit = self.lit;

        let TransformSnippets {
            m_id,
            muttest_api,
            loc,
        } = transformer.new_mutable(&self, &self.c.to_string());
        quote_spanned! {span=>
            #muttest_api::mutable::lit_char::run(#m_id, #lit, #loc)
        }
    }
    fn mutations(analysis: &crate::report::MutableAnalysis) -> Vec<String> {
        // TODO: configure this at runtime
        ["A", "0"].into_iter().filter_mutable_code(&analysis.code)
    }
}

#[cfg_attr(feature = "selftest", muttest::mutate)]
pub fn run(m_id: BakedMutableId, c: char, loc: BakedLocation) -> char {
    let mutation = m_id.get_active_mutation();
    if mutation.is_skip() {
        return c;
    }

    mutation.report_coverage(None);

    mutation.report_details(loc, "char", "");

    match mutation.as_option() {
        None => c,
        Some(p) => {
            if p.chars().count() != 1 {
                todo!();
            }
            p.chars().next().unwrap()
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    #[test]
    fn single_lit() {
        #[muttest_codegen::mutate_isolated("lit_char")]
        fn f() -> char {
            'A'
        }

        let data = data_isolated!(f);
        assert_eq!(data.mutables.len(), 1);

        assert_eq!(call_isolated! {f()}.res, 'A');
        assert_eq!(call_isolated! {f() where 1 => "B"}.res, 'B');
        assert_eq!(call_isolated! {f() where 1 => "🦀"}.res, '🦀');
    }

    #[test]
    fn push_char() {
        #[muttest_codegen::mutate_isolated("lit_char")]
        fn f(s: &mut String) {
            s.push('X')
        }

        let data = data_isolated!(f);
        assert_eq!(data.mutables.len(), 1);

        let mut s = String::new();
        call_isolated! {f(&mut s)};
        assert_eq!(s, "X");
        call_isolated! {f(&mut s) where 1 => "B"};
        assert_eq!(s, "XB");
        call_isolated! {f(&mut s) where 1 => "🦀"};
        assert_eq!(s, "XB🦀");
    }

    // TODO: also test invalid mutations

    #[test]
    fn consts_not_mutated() {
        #[muttest_codegen::mutate_isolated("lit_char")]
        fn _f() {
            const X1: char = 'A';
            static X2: char = 'B';
            const fn c() -> char {
                'X'
            }
        }
        let data = data_isolated!(_f);
        assert_eq!(data.mutables.len(), 0);
    }

    #[test]
    fn patterns_not_mutated() {
        #[muttest_codegen::mutate_isolated("lit_char")]
        fn _f(c: char) {
            match c {
                'a' => {}
                '0'..='9' => {}
                _ => {}
            }
        }
        let data = data_isolated!(_f);
        assert_eq!(data.mutables.len(), 0);
    }
}
