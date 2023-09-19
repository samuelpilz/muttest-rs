use std::{iter, ops::ControlFlow};

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
        if let Some(len) = &analysis.ty {
            // byte string
            let Ok(len) = len.parse::<usize>() else {
                // TODO: return Result instead
                return vec![];
            };
            vec!['A', '0']
                .into_iter()
                .map(|c| iter::repeat(c).take(len).collect::<String>())
                .filter_mutable_code(&analysis.code)
        } else {
            // normal string
            ["", "A", "0"]
                .into_iter()
                .filter_mutable_code(&analysis.code)
        }
    }

    #[cfg_attr(muttest, ignore)]
    fn transform_expr(
        &self,
        transformer: &mut MuttestTransformer,
        e: &Expr,
    ) -> ControlFlow<Mutated> {
        let mutable = match e {
            Expr::Lit(ExprLit {
                lit: Lit::Str(lit),
                attrs,
            }) if attrs.is_empty() => Mutable {
                code: format!("{:?}", lit.value()),
                tokens: lit.to_token_stream(),
                span: lit.span(),
                len: None,
            },
            Expr::Lit(ExprLit {
                lit: Lit::ByteStr(lit),
                attrs,
            }) if attrs.is_empty() => Mutable {
                code: format!("{:?}", lit.token().to_string()),
                tokens: lit.to_token_stream(),
                span: lit.span(),
                len: Some(lit.value().len()),
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
    len: Option<usize>,
}

impl Mutable {
    fn transform(self, transformer: &mut MuttestTransformer) -> Mutated {
        let span = self.span;
        let TransformSnippets { m_id, muttest_api } = transformer.new_mutable(span);
        let tokens = self.tokens;

        let transformed = quote_spanned! {span=>
            // TODO: detect if fixed-len byte-str?
            #muttest_api::mutator::lit_str::run(#m_id, #tokens)
        };
        Mutated {
            transformed,
            code: self.code,
            // TODO: write types in better style
            type_info: self.len.map(|l| l.to_string()),
            span,
        }
    }
}

#[cfg_attr(feature = "selftest", muttest::mutate)]
pub fn run<T: StrMutable>(m_id: MutableId, original: T) -> T {
    m_id.write_coverage();

    let Some(m) = m_id.get_action() else {
        return original;
    };
    match <T as StrMutable>::from_str(m) {
        Ok(m) => m,
        Err(e) => internal_error(e),
    }
}
pub trait StrMutable: Sized {
    fn from_str(s: &'static str) -> Result<Self, Error>;
}
impl StrMutable for &'static str {
    fn from_str(s: &'static str) -> Result<Self, Error> {
        Ok(s)
    }
}
impl StrMutable for &'static [u8] {
    fn from_str(s: &'static str) -> Result<Self, Error> {
        Ok(s.as_bytes())
    }
}
impl<const N: usize> StrMutable for &'static [u8; N] {
    fn from_str(s: &'static str) -> Result<Self, Error> {
        s.as_bytes()
            .try_into()
            .map_err(|_| Error::InvalidMutation(s))
    }
}

#[cfg(test)]
#[cfg_attr(feature = "selftest", muttest::tests)]
mod tests {

    use super::Mutator;
    use crate::{mutator::MutatorFns, tests::*};

    #[test]
    fn empty_str() {
        #[muttest_codegen::mutate_isolated("lit_str")]
        fn f() -> &'static str {
            ""
        }
        let data = data_isolated!(f);
        assert_eq!(data.mutables.len(), 1);

        assert_eq!(call_isolated! {f()}.res, "");
        assert_eq!(call_isolated! {f() where 1: "x"}.res, "x");
    }

    #[test]
    fn some_str() {
        #[muttest_codegen::mutate_isolated("lit_str")]
        fn f() -> &'static str {
            "mutation testing!"
        }
        let data = data_isolated!(f);
        assert_eq!(data.mutables.len(), 1);

        assert_eq!(call_isolated! {f()}.res, "mutation testing!");
        assert_eq!(call_isolated! {f() where 1: ""}.res, "");
    }

    #[test]
    fn some_byte_str() {
        #[muttest_codegen::mutate_isolated("lit_str")]
        fn f() -> &'static [u8] {
            b"\xff\xff"
        }
        let data = data_isolated!(f);
        assert_eq!(data.mutables.len(), 1);

        assert_eq!(call_isolated! {f()}.res, b"\xff\xff");
        assert_eq!(call_isolated! {f() where 1: ""}.res, b"");
    }

    #[test]
    fn fixed_len_byte_str() {
        #[muttest_codegen::mutate_isolated("lit_str")]
        fn f() -> &'static [u8; 2] {
            b"\xff\xff"
        }
        let data = data_isolated!(f);
        assert_eq!(data.mutables.len(), 1);

        assert_eq!(call_isolated! {f()}.res, b"\xff\xff");
        assert_eq!(call_isolated! {f() where 1: "\0\0"}.res, b"\0\0");
    }

    #[test]
    fn patterns_not_mutated() {
        #[muttest_codegen::mutate_isolated("lit_str")]
        fn _f(x: String) {
            match &*x {
                "a" => {}
                "xyz" => {}
                _ => {}
            }
        }
        let data = data_isolated!(_f);
        assert_eq!(data.mutables.len(), 0);
    }

    #[test]
    fn no_empty_string_mutation_for_empty_string() {
        #[muttest_codegen::mutate_isolated("lit_str")]
        fn _f() -> &'static str {
            ""
        }
        let report = data_isolated!(_f);
        assert_eq!(report.mutables.len(), 1);

        let mutations = Mutator.valid_mutations(&report.mutables.values().next().unwrap().analysis);

        for m in mutations {
            assert_ne!(m, r#""""#);
        }
    }
}
