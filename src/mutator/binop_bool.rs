use std::{fmt, ops::ControlFlow};

use quote::{quote_spanned, ToTokens};
use syn::{spanned::Spanned, BinOp, Expr, ExprBinary};

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
        ["&&", "||"].filter_mutable_code(&analysis.code)
    }

    #[cfg_attr(muttest, ignore)]
    fn transform_expr(
        &self,
        transformer: &mut MuttestTransformer,
        e: &Expr,
    ) -> ControlFlow<Mutated> {
        let Expr::Binary(ExprBinary {
            left, op, right, ..
        }) = e
        else {
            return ControlFlow::Continue(());
        };
        if !matches!(op, BinOp::And(_) | BinOp::Or(_)) {
            return ControlFlow::Continue(());
        }

        let span = op.span();
        let op_str = op.to_token_stream().to_string();
        let TransformSnippets { m_id, muttest_api } = transformer.new_mutable(span);

        ControlFlow::Break(Mutated {
            transformed: quote_spanned! {span=>
                #muttest_api::id(#[allow(unused)]{
                    (#m_id).write_types("bool", "");
                    match #muttest_api::mutator::binop_bool::run_left(#m_id, #op_str, #left) {
                        #muttest_api::ControlFlow::Break(__muttest_b) => __muttest_b,
                        #muttest_api::ControlFlow::Continue(__muttest_b) => {
                            #muttest_api::mutator::binop_bool::run_right(#m_id, __muttest_b, #right)
                        }
                    }
                })
            },
            code: op_str,
            span,
            type_info: None,
        })
    }
}

#[cfg_attr(feature = "selftest", muttest::mutate)]
pub fn run_left(m_id: MutableId, op_str: &'static str, left: bool) -> ControlFlow<bool, bool> {
    debug_assert!(matches!(op_str, "&&" | "||"));

    if left == (op_str == "&&") {
        m_id.write_coverage()
    } else {
        m_id.write_coverage_behavior(BoolOpDisplay { left, right: None })
    }

    match (left, m_id.get_action().unwrap_or(op_str)) {
        (false, "&&") | (true, "||") => ControlFlow::Break(left),
        (true, "&&") | (false, "||") => ControlFlow::Continue(left),
        (_, m) => internal_error(Error::InvalidMutation(m)),
    }
}

#[cfg_attr(feature = "selftest", muttest::mutate)]
pub fn run_right(m_id: MutableId, left: bool, right: bool) -> bool {
    m_id.write_coverage_behavior(BoolOpDisplay {
        left,
        right: Some(right),
    });
    right
}
fn bool_to_str(left: bool, right: Option<bool>) -> &'static str {
    match (left, right) {
        (true, None) => "T",
        (true, Some(true)) => "TT",
        (true, Some(false)) => "TF",
        (false, None) => "F",
        (false, Some(true)) => "FT",
        (false, Some(false)) => "FF",
    }
}
struct BoolOpDisplay {
    left: bool,
    right: Option<bool>,
}
impl fmt::Display for BoolOpDisplay {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", bool_to_str(self.left, self.right))
    }
}

#[cfg(test)]
#[cfg_attr(feature = "selftest", muttest::tests)]
mod tests {
    use super::Mutator;
    use crate::{mutator::MutatorFns, tests::*};

    #[test]
    fn true_and_false() {
        #[muttest_codegen::mutate_isolated("binop_bool")]
        fn f() -> bool {
            true && false
        }
        assert_eq!(
            Mutator
                .valid_mutations(&data_isolated!(f).for_mutable(1).analysis)
                .to_vec_deref(),
            vec!["||"]
        );

        let res = call_isolated! {f()};
        assert_eq!(false, res.res);
        assert_eq!(&res.report.for_mutable(1).analysis.behavior(), &["TF"]);
    }

    #[test]
    fn true_or_false() {
        #[muttest_codegen::mutate_isolated("binop_bool")]
        fn f() -> bool {
            true || false
        }
        assert_eq!(
            Mutator
                .valid_mutations(&data_isolated!(f).for_mutable(1).analysis)
                .to_vec_deref(),
            vec!["&&"]
        );

        let res = call_isolated! {f()};
        assert_eq!(true, res.res);
        assert_eq!(&res.report.for_mutable(1).analysis.behavior(), &["T"]);
    }
}
