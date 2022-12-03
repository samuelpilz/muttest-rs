use proc_macro2::{Span, TokenStream};
use quote::{quote_spanned, ToTokens};
use syn::{spanned::Spanned, BinOp, Expr, ExprBinary};

use crate::{
    report::MutableAnalysis,
    transformer::{MuttestTransformer, TransformSnippets},
    Mutation,
};

use super::{FilterMutableCode, MatchMutable};

pub struct Mutable<'a> {
    pub left: &'a dyn ToTokens,
    pub right: &'a dyn ToTokens,
    pub op: &'a dyn ToTokens,
    pub span: Span,
}

impl<'a> MatchMutable<'a, Expr> for Mutable<'a> {
    fn try_match<'b: 'a>(expr: &'b Expr) -> Option<Self> {
        match expr {
            Expr::Binary(ExprBinary {
                left, op, right, ..
            }) if matches!(op, BinOp::Eq(_) | BinOp::Ne(_)) => Some(Self {
                left,
                right,
                op,
                span: op.span(),
            }),
            _ => None,
        }
    }
}

impl<'a> super::Mutable<'a> for Mutable<'a> {
    const NAME: &'static str = "binop_eq";

    fn span(&self) -> Span {
        self.span
    }

    fn transform(self, transformer: &mut MuttestTransformer) -> TokenStream {
        let span = self.span;
        let op = self.op.to_token_stream();
        let op_str = op.to_string();
        let (left, right) = (self.left, self.right);

        let TransformSnippets {
            m_id,
            muttest_api,
            loc,
        } = transformer.new_mutable(&self, &op_str);

        quote_spanned! {span=>
            #muttest_api::id({
                let __muttest_mutation = (#m_id).get_active_mutation();
                if __muttest_mutation.is_skip() {
                    (#left) #op (#right)
                } else {
                    __muttest_mutation.report_details(#loc,"","");

                    // for improved type-inference and `!`-type handling call the eq-operation here.
                    let _res = #left #op #right;

                    #muttest_api::mutable::binop_eq::run(__muttest_mutation, #op_str, _res)
                }
            })
        }
    }

    fn mutations(analysis: &MutableAnalysis) -> Vec<String> {
        ["==", "!="].filter_mutable_code(&analysis.code)
    }
}

#[cfg_attr(feature = "selftest", muttest::mutate)]
pub fn run(mutation: Mutation, op_str: &str, res: bool) -> bool {
    debug_assert!(matches!(op_str, "==" | "!="));

    let eq = (op_str == "==") == res;

    // this reports behavior but is irrelevant for weak mutation testing
    mutation.report_coverage(Some(if eq { "EQ" } else { "NE" }));

    match mutation.as_option().unwrap_or(op_str) {
        "==" => eq,
        "!=" => !eq,
        _ => todo!(),
    }
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    #[test]
    fn eq_ints() {
        #[muttest_codegen::mutate_isolated("binop_eq")]
        fn f() -> bool {
            1 == 2
        }
        let res = call_isolated! {f()};
        assert_eq!(false, res.res);
        assert_eq!(
            &res.report
                .for_mutable(1)
                .analysis
                .behavior
                .iter()
                .collect::<Vec<_>>(),
            &["NE"]
        );
        assert_eq!(true, call_isolated! {f() where 1 => "!="}.res);
    }

    #[test]
    fn ne_ints() {
        #[muttest_codegen::mutate_isolated("binop_eq")]
        fn f() -> bool {
            1 != 2
        }
        let res = call_isolated! {f()};
        assert_eq!(true, res.res);
        assert_eq!(
            &res.report
                .for_mutable(1)
                .analysis
                .behavior
                .iter()
                .collect::<Vec<_>>(),
            &["NE"]
        );
        assert_eq!(false, call_isolated! {f() where 1 => "=="}.res);
    }

    #[test]
    fn compare_unsized() {
        #[muttest_codegen::mutate_isolated("binop_eq")]
        fn f(s: String) -> bool {
            *s == *"2";
            // second call to ensure that s is not moved in call before
            *s == *"2"
        }

        assert_eq!(call_isolated! {f(String::new())}.res, false);
        assert_eq!(call_isolated! {f(String::new()) where 2 => "!="}.res, true);
    }

    #[test]
    fn compare_tmp_vars() {
        #[muttest_codegen::mutate_isolated("binop_eq")]
        fn f() -> bool {
            vec!["1", "2", "3"].into_iter().collect::<String>() == "123".to_owned()
        }

        assert_eq!(call_isolated! {f()}.res, true);
        assert_eq!(call_isolated! {f() where 1 => "!="}.res, false);
    }

    #[test]
    fn details_reported_before_covered() {
        #[muttest_codegen::mutate_isolated("binop_eq")]
        #[allow(unreachable_code)]
        fn f() -> u8 {
            return loop {
                if ({
                    break 1;
                    2
                }) == 2
                {
                    return 3;
                } else {
                    return 4;
                };
            };
        }

        let res = call_isolated! {f()};
        assert_eq!(1, res.res);
        assert_ne!(res.report.for_mutable(1).location.module, None);
        assert_eq!(res.report.for_mutable(1).analysis.covered, false);
    }

    #[test]
    fn assign_as_expr() {
        #[muttest_codegen::mutate_isolated("binop_eq")]
        fn f() {
            let mut _x = 1;
            let b = (_x = 2) == (_x = 3);
            assert!(b);
        }

        call_isolated! {f()};
    }

    #[test]
    fn never_type_expr() {
        #[muttest_codegen::mutate_isolated("binop_eq")]
        #[allow(unreachable_code)]
        fn f() {
            let _a = () == return;
            panic!();
        }

        call_isolated! {f()};
    }
}
