use std::ops::ControlFlow;

use proc_macro2::{Span, TokenStream};
use quote::{quote_spanned, ToTokens};
use syn::{Expr, ExprLit, Lit};

use crate::{
    internal_error,
    report::{MutableAnalysis, MutableId},
    transformer::{Mutated, MuttestTransformer, TransformSnippets},
    Error,
};

use super::{FilterMutableCode, MutatorFns};

pub struct Mutator;

#[cfg_attr(feature = "selftest", muttest::mutate)]
impl MutatorFns for Mutator {
    fn valid_mutations(&self, analysis: &MutableAnalysis) -> Vec<String> {
        ["A", "0"].into_iter().filter_mutable_code(&analysis.code)
    }

    #[cfg_attr(muttest, ignore)]
    fn transform_expr(
        &self,
        transformer: &mut MuttestTransformer,
        e: &Expr,
    ) -> ControlFlow<Mutated> {
        let mutable = match e {
            Expr::Lit(ExprLit {
                lit: Lit::Char(lit),
                ..
            }) => Mutable {
                code: lit.value().escape_default().to_string(),
                tokens: lit.to_token_stream(),
                span: lit.span(),
            },
            Expr::Lit(ExprLit {
                lit: Lit::Byte(lit),
                ..
            }) => Mutable {
                code: lit.value().to_string(),
                tokens: lit.to_token_stream(),
                span: lit.span(),
            },
            _ => return ControlFlow::Continue(()),
        };
        ControlFlow::Break(mutable.transform(transformer))
    }
}

struct Mutable {
    code: String,
    tokens: TokenStream,
    span: Span,
}
impl Mutable {
    fn transform(self, transformer: &mut MuttestTransformer) -> Mutated {
        let span = self.span;
        let TransformSnippets { m_id, muttest_api } = transformer.new_mutable(span);
        let tokens = &self.tokens;

        Mutated {
            transformed: quote_spanned! {span=>
                #muttest_api::mutator::lit_char::run(#m_id, #tokens)
            },
            code: self.code,
            span,
            type_info: None,
        }
    }
}

#[cfg_attr(feature = "selftest", muttest::mutate)]
pub fn run<T: CharMutable>(m_id: MutableId, original: T) -> T {
    m_id.write_coverage();

    let Some(m) = m_id.get_action() else {
        return original;
    };
    match <T as CharMutable>::from_str(m) {
        Ok(m) => m,
        Err(e) => internal_error(e),
    }
}

pub trait CharMutable: Sized {
    fn from_str(s: &'static str) -> Result<Self, Error>;
}
impl CharMutable for char {
    fn from_str(s: &'static str) -> Result<Self, Error> {
        if s.len() > 4 || s.chars().count() != 1 {
            Err(Error::InvalidMutation(s))
        } else {
            Ok(s.chars().next().unwrap())
        }
    }
}
impl CharMutable for u8 {
    fn from_str(s: &'static str) -> Result<Self, Error> {
        if s.len() != 1 {
            Err(Error::InvalidMutation(s))
        } else {
            Ok(s.as_bytes()[0])
        }
    }
}

#[cfg(test)]
#[cfg_attr(feature = "selftest", muttest::tests)]
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
        assert_eq!(call_isolated! {f() where 1: "B"}.res, 'B');
        assert_eq!(call_isolated! {f() where 1: "🦀"}.res, '🦀');
    }

    #[test]
    fn byte_lit() {
        #[muttest_codegen::mutate_isolated("lit_char")]
        fn f() -> u8 {
            b'A'
        }

        let data = data_isolated!(f);
        assert_eq!(data.mutables.len(), 1);

        assert_eq!(call_isolated! {f()}.res, b'A');
        assert_eq!(call_isolated! {f() where 1: "B"}.res, b'B');
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
        call_isolated! {f(&mut s) where 1: "B"};
        assert_eq!(s, "XB");
        call_isolated! {f(&mut s) where 1: "🦀"};
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
