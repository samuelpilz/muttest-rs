use std::ops::ControlFlow;

use quote::quote_spanned;
use syn::{Expr, ExprLit, Lit};

use crate::{
    internal_error,
    report::{MutableAnalysis, MutableId},
    transformer::{self, Mutated, MuttestTransformer, TransformSnippets},
    Error,
};

use super::{FilterMutableCode, MutatorFns};

pub struct Mutator;

#[cfg_attr(feature = "selftest", muttest::mutate)]
impl MutatorFns for Mutator {
    fn valid_mutations(&self, analysis: &MutableAnalysis) -> Vec<String> {
        ["true", "false"]
            .into_iter()
            .filter_mutable_code(&analysis.code)
    }

    #[cfg_attr(muttest, ignore)]
    fn transform_expr(
        &self,
        transformer: &mut MuttestTransformer,
        e: &Expr,
    ) -> ControlFlow<transformer::Mutated> {
        let Expr::Lit(ExprLit {
            lit: Lit::Bool(lit),
            ..
        }) = e
        else {
            return ControlFlow::Continue(());
        };
        let span = lit.span();

        let TransformSnippets { m_id, muttest_api } = transformer.new_mutable(span);

        ControlFlow::Break(Mutated {
            transformed: quote_spanned! {span=>
                #muttest_api::mutator::lit_bool::run(#m_id, #lit)
            },
            code: format!("{:?}", lit.value()),
            span,
            type_info: None,
        })
    }
}

#[cfg_attr(feature = "selftest", muttest::mutate)]
pub fn run(m_id: MutableId, b: bool) -> bool {
    m_id.write_coverage();
    match m_id.get_action() {
        None => b,
        Some("true") => true,
        Some("false") => false,
        Some(m) => internal_error(Error::InvalidMutation(m)),
    }
}

#[cfg(test)]
#[cfg_attr(feature = "selftest", muttest::tests)]
mod tests {

    use super::Mutator;
    use crate::{mutator::MutatorFns, tests::*};

    #[test]
    fn ret_true() {
        #[muttest_codegen::mutate_isolated("lit_bool")]
        fn f() -> bool {
            true
        }
        let data = data_isolated!(f);
        assert_eq!(data.mutables.len(), 1);
        assert_eq!(
            Mutator
                .valid_mutations(&data.for_mutable(1).analysis)
                .to_vec_deref(),
            vec!["false"]
        );

        assert_eq!(call_isolated! {f()}.res, true);
        assert_eq!(call_isolated! {f() where 1: "false"}.res, false);
    }

    #[test]
    fn ret_false() {
        #[muttest_codegen::mutate_isolated("lit_bool")]
        fn f() -> bool {
            false
        }
        let data = data_isolated!(f);
        assert_eq!(data.mutables.len(), 1);
        assert_eq!(
            Mutator
                .valid_mutations(&data.for_mutable(1).analysis)
                .to_vec_deref(),
            vec!["true"]
        );

        assert_eq!(call_isolated! {f()}.res, false);
        assert_eq!(call_isolated! {f() where 1: "true"}.res, true);
    }
}
