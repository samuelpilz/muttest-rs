use std::ops::ControlFlow;

use quote::quote_spanned;
use syn::{Expr, ExprBreak};

use crate::{
    internal_error,
    report::{MutableAnalysis, MutableId},
    transformer::{Mutated, MuttestTransformer, TransformSnippets},
    Error,
};

use super::MutatorFns;

pub struct Mutator;

impl MutatorFns for Mutator {
    fn valid_mutations(&self, _: &MutableAnalysis) -> Vec<String> {
        vec!["continue".to_string()]
    }
    fn transform_expr(
        &self,
        transformer: &mut MuttestTransformer,
        e: &Expr,
    ) -> ControlFlow<crate::transformer::Mutated> {
        let Expr::Break(ExprBreak {
            break_token,
            label,
            expr,
            ..
        }) = e
        else {
            return ControlFlow::Continue(());
        };
        // TODO: allow valued or labeled breaks
        // note that labelled breaks can be block-breaks instead of loop-breaks
        if expr.is_some() || label.is_some() {
            return ControlFlow::Continue(());
        }

        let span = break_token.span;
        let TransformSnippets { m_id, muttest_api } = transformer.new_mutable(span);
        ControlFlow::Break(Mutated {
            transformed: quote_spanned! {span=>
                {
                    match #muttest_api::mutator::loop_break::run(#m_id) {
                        #muttest_api::mutator::loop_break::Mutation::Break => break #label,
                        #muttest_api::mutator::loop_break::Mutation::Continue => continue #label,
                    }
                }
            },
            code: "break".to_string(),
            span,
            type_info: None,
        })
    }
}

// TODO: add no-op mutation if possible
pub enum Mutation {
    Break,
    Continue,
}

#[cfg_attr(feature = "selftest", muttest::mutate)]
pub fn run(m_id: MutableId) -> Mutation {
    m_id.write_coverage();

    match m_id.get_action() {
        None => Mutation::Break,
        Some("continue") => Mutation::Continue,
        Some(m) => internal_error(Error::InvalidMutation(m)),
    }
}

#[cfg(test)]
#[cfg_attr(feature = "selftest", muttest::tests)]
mod tests {

    use super::Mutator;
    use crate::{mutator::MutatorFns, tests::*};

    #[test]
    fn mul_to_first_zero() {
        #[muttest_codegen::mutate_isolated("loop_break")]
        fn f() -> i32 {
            let mut sum = 1;
            for x in [2, 0, 3] {
                if x == 0 {
                    break;
                }
                sum *= x
            }
            sum
        }
        assert_eq!(
            Mutator
                .valid_mutations(&data_isolated!(f).for_mutable(1).analysis)
                .to_vec_deref(),
            vec!["continue"]
        );

        let res = call_isolated! {f()};
        assert_eq!(res.res, 2);
        let res = call_isolated! {f() where 1: "continue"};
        assert_eq!(res.res, 6);
    }
}
