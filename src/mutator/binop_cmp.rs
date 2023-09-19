use std::{cmp::Ordering, collections::BTreeSet, ops::ControlFlow};

use quote::{quote_spanned, ToTokens};
use syn::{spanned::Spanned, BinOp, Expr, ExprBinary};

use crate::{
    report::{MutableAnalysis, MutableId},
    transformer::{Mutated, MuttestTransformer, TransformSnippets},
};

use super::{eval_ord, ord_from_str, ord_to_str, FilterMutableCode, MutatorFns};

pub struct Mutator;

#[cfg_attr(feature = "selftest", muttest::mutate)]
impl MutatorFns for Mutator {
    fn valid_mutations(&self, analysis: &MutableAnalysis) -> Vec<String> {
        ["<", "<=", "==", "!=", ">=", ">"]
            .into_iter()
            .filter_mutable_code(&analysis.code)
    }

    fn identical_behavior(&self, code: &str, behavior: &BTreeSet<String>, mutation: &str) -> bool {
        behavior
            .iter()
            .map(|ord| ord_from_str(ord))
            .all(|ord| eval_ord(ord, code) == eval_ord(ord, mutation))
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
        if !matches!(
            op,
            BinOp::Lt(_) | BinOp::Le(_) | BinOp::Ge(_) | BinOp::Gt(_)
        ) {
            return ControlFlow::Continue(());
        }

        let span = op.span();
        let op_str = op.to_token_stream().to_string();
        let TransformSnippets { m_id, muttest_api } = transformer.new_mutable(span);

        ControlFlow::Break(Mutated {
            transformed: quote_spanned! {span=>
                #muttest_api::id(#[allow(unused)]{
                    (#m_id).write_types("", "");

                    let (left, right) = (&(#left), &(#right));

                    // this is required for handling comparisons where one side has type `!`
                    use #muttest_api::mutator::binop_cmp::is_partial_ord::IsOrd;
                    let ord = (&&(left, right)).get_ord();
                    // TODO: this hides type errors if there are compiler errors in the original code
                    // TODO: make compilefail test

                    #muttest_api::mutator::binop_cmp::run(#m_id, #op_str, ord)
                })
            },
            code: op_str,
            span,
            type_info: None,
        })
    }
}

#[cfg_attr(feature = "selftest", muttest::mutate)]
pub fn run(m_id: MutableId, op_str: &'static str, ord: Option<Ordering>) -> bool {
    m_id.write_coverage_behavior(ord_to_str(ord));

    super::eval_ord(ord, m_id.get_action().unwrap_or(op_str))
}

pub mod is_partial_ord {
    use std::cmp::Ordering;

    use crate::{internal_error, Error};

    pub trait IsOrd<L: ?Sized, R: ?Sized> {
        fn is_ord(&self) -> bool;
        fn get_ord(&self) -> Option<Ordering>;
    }
    impl<L: PartialOrd<R> + ?Sized, R: ?Sized> IsOrd<L, R> for &(&L, &R) {
        fn is_ord(&self) -> bool {
            true
        }
        fn get_ord(&self) -> Option<Ordering> {
            <L as PartialOrd<R>>::partial_cmp(self.0, self.1)
        }
    }
    impl<L: ?Sized, R: ?Sized> IsOrd<L, R> for (&L, &R) {
        fn is_ord(&self) -> bool {
            false
        }

        fn get_ord(&self) -> Option<Ordering> {
            internal_error(Error::InvalidSpecialization(module_path!()))
        }
    }
}

#[cfg(test)]
#[cfg_attr(feature = "selftest", muttest::tests)]
mod tests {
    use super::*;
    use crate::tests::*;

    #[test]
    fn lt_ints() {
        #[muttest_codegen::mutate_isolated("binop_cmp")]
        fn f() -> bool {
            1 < 2
        }

        let data = data_isolated!(f);
        assert_eq!(data.mutables.len(), 1);

        let res = call_isolated! {f()};
        assert_eq!(true, res.res);
        assert_eq!(&res.report.for_mutable(1).analysis.behavior(), &["LT"]);
        assert_eq!(false, call_isolated! {f() where 1: ">"}.res);
        assert_eq!(false, call_isolated! {f() where 1: ">="}.res);
    }

    #[test]
    fn count_up() {
        #[muttest_codegen::mutate_isolated("binop_cmp")]
        fn f() -> u8 {
            let mut x = 1;
            while x < 2 {
                x += 1;
            }
            x
        }

        let res = call_isolated! {f()};
        assert_eq!(2, res.res);
        assert_eq!(
            &res.report.for_mutable(1).analysis.behavior(),
            &["EQ", "LT"]
        );
        let res = call_isolated! {f() where 1: "<="};
        assert_eq!(3, res.res);
    }

    #[test]
    fn compare_unsized() {
        #[muttest_codegen::mutate_isolated("binop_cmp")]
        fn f(s: String) -> bool {
            *s < *"2";
            // second call to ensure that s is not moved in call before
            *s < *"2"
        }

        assert_eq!(call_isolated! {f(String::new())}.res, true);
        assert_eq!(call_isolated! {f(String::new()) where 2: ">"}.res, false);
    }

    #[test]
    fn compare_tmp_vars() {
        #[muttest_codegen::mutate_isolated("binop_cmp")]
        fn f() -> bool {
            vec!["1", "2", "3"].into_iter().collect::<String>() < "123".to_owned()
        }

        assert_eq!(call_isolated! {f()}.res, false);
        assert_eq!(call_isolated! {f() where 1: ">"}.res, false);
    }

    #[test]
    fn inference_is_ok() {
        #[muttest_codegen::mutate_isolated("binop_cmp")]
        fn f() -> bool {
            1 < 2u8
        }
        let res = call_isolated! {f()};
        assert_eq!(true, res.res);
        assert_eq!(&res.report.for_mutable(1).analysis.behavior(), &["LT"]);
    }

    #[test]
    fn partial_cmp_gives_none() {
        #[muttest_codegen::mutate_isolated("binop_cmp")]
        fn f() -> bool {
            1.0 < f64::NAN
        }
        let res = call_isolated! {f()};
        assert_eq!(false, res.res);
        assert_eq!(&res.report.for_mutable(1).analysis.behavior(), &["-"]);

        // all mutations give false
        for m in ["<=", ">=", ">"] {
            assert_eq!(call_isolated! {f() where 1: m}.res, false);
        }
    }

    #[test]
    fn assign_as_expr() {
        #[muttest_codegen::mutate_isolated("binop_cmp")]
        fn f() {
            let mut _x = 1;
            let b = (_x = 2) <= (_x = 3);
            assert!(b);
        }
        call_isolated! {f()};
    }

    #[test]
    fn never_type_expr() {
        #[muttest_codegen::mutate_isolated("binop_cmp")]
        #[allow(unreachable_code)]
        fn f() {
            let _a = () < return;
            panic!();
        }
        call_isolated! {f()};
        // assert that panic is not reached
    }

    #[test]
    fn identical_behavior_ints() {
        #[muttest_codegen::mutate_isolated("binop_cmp")]
        #[allow(unreachable_code)]
        fn f(a: i32, b: i32) {
            a < b;
        }
        let res = call_isolated!(f(1, 2));
        let (identical, non_identical) =
            eval_identical_behavior_mutations(&Mutator, &res.report.for_mutable(1).analysis);
        assert_eq!(identical.to_vec_deref(), vec!["<=", "!="].sorted());
        assert_eq!(non_identical.to_vec_deref(), vec![">", ">=", "=="].sorted());
    }

    #[test]
    fn identical_behavior_nans() {
        #[muttest_codegen::mutate_isolated("binop_cmp")]
        #[allow(unreachable_code)]
        fn f(a: f32, b: f32) {
            a < b;
        }
        let res = call_isolated!(f(f32::NAN, f32::NAN));
        let (identical, non_identical) =
            eval_identical_behavior_mutations(&Mutator, &res.report.for_mutable(1).analysis);
        assert_eq!(
            identical.to_vec_deref(),
            vec!["<=", ">", ">=", "=="].sorted()
        );
        assert_eq!(non_identical.to_vec_deref(), vec!["!="].sorted());
    }

    #[test]
    fn no_identical_behavior() {
        #[muttest_codegen::mutate_isolated("binop_cmp")]
        #[allow(unreachable_code)]
        fn f() {
            for x in [1, 2, 3] {
                x < 2;
            }
        }
        let res = call_isolated!(f());
        let (identical, non_identical) =
            eval_identical_behavior_mutations(&Mutator, &res.report.for_mutable(1).analysis);
        assert_eq!(identical.to_vec_deref(), <Vec<&str>>::new());
        assert_eq!(
            non_identical.to_vec_deref(),
            vec!["!=", "<=", ">", ">=", "=="].sorted()
        );
    }
}
