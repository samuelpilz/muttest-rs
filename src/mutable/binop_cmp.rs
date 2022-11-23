use std::cmp::Ordering;

use proc_macro2::{Span, TokenStream};
use quote::{quote_spanned, ToTokens};

use crate::{
    report::MutableAnalysis,
    transformer::{MuttestTransformer, TransformSnippets},
    Mutation,
};

use super::FilterMutableCode;

pub struct Mutable<'a> {
    pub left: &'a dyn ToTokens,
    pub right: &'a dyn ToTokens,
    pub op: &'a dyn ToTokens,
    pub span: Span,
}

impl<'a> super::Mutable<'a> for Mutable<'a> {
    const NAME: &'static str = "binop_cmp";

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

                    __muttest_mutation.report_details(#loc, "", "");

                    let (_left, _right) = (&(#left), &(#right));

                    // this is required for handling comparisons where one side has type `!`
                    #[allow(unused_imports)]
                    use #muttest_api::mutable::binop_cmp::{IsNo, IsYes};
                    let ord = (&(&_left, &_right))
                        .get_impl()
                        .run(_left, _right);

                    #muttest_api::mutable::binop_cmp::run(__muttest_mutation, #op_str, ord)
                }
            })
        }
    }

    fn mutations(analysis: &MutableAnalysis) -> Vec<String> {
        ["<", "<=", ">=", ">"]
            .into_iter()
            .filter_mutable_code(&analysis.code)
    }

    fn identical_behavior(analysis: &MutableAnalysis, mutation: &str) -> bool {
        fn eval(op: &str, ord: &str) -> bool {
            match op {
                _ if ord.is_empty() => false,
                "<" => ord == "LT",
                "<=" => ord != "GT",
                ">=" => ord != "LT",
                ">" => ord == "GT",
                _ => unimplemented!(),
            }
        }
        analysis
            .behavior
            .iter()
            .all(|ord| eval(&analysis.code, ord) == eval(mutation, ord))
    }
}

#[cfg_attr(test, muttest_codegen::mutate_selftest)]
pub fn run(mutation: Mutation, op_str: &str, ord: Option<Ordering>) -> bool {
    mutation.report_coverage(Some(match ord {
        None => "",
        Some(Ordering::Less) => "LT",
        Some(Ordering::Equal) => "EQ",
        Some(Ordering::Greater) => "GT",
    }));

    match (ord, mutation.as_option().unwrap_or(op_str)) {
        (None, _) => false,
        (Some(ord), "<") => ord.is_lt(),
        (Some(ord), "<=") => ord.is_le(),
        (Some(ord), ">=") => ord.is_ge(),
        (Some(ord), ">") => ord.is_gt(),
        _ => todo!(),
    }
}

pub struct Yes;
pub struct No;

pub trait IsYes {
    fn get_impl(&self) -> Yes;
}
pub trait IsNo {
    fn get_impl(&self) -> No;
}
impl<L: PartialOrd<R> + ?Sized, R: ?Sized> IsYes for (&L, &R) {
    fn get_impl(&self) -> Yes {
        Yes
    }
}
impl<L: ?Sized, R: ?Sized> IsNo for &(&L, &R) {
    fn get_impl(&self) -> No {
        No
    }
}
impl Yes {
    pub fn is_impl(&self) -> bool {
        true
    }
    pub fn run<L: PartialOrd<R> + ?Sized, R: ?Sized>(
        self,
        left: &L,
        right: &R,
    ) -> Option<Ordering> {
        <L as PartialOrd<R>>::partial_cmp(left, right)
    }
}
impl No {
    pub fn is_impl(&self) -> bool {
        false
    }
    pub fn run<L, R, O>(self, _: L, _: R) -> O {
        unreachable!()
    }
}

#[cfg(test)]
mod tests {
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
        assert_eq!(
            &res.report.for_mutable(1).analysis.behavior.to_vec_ref(),
            &["LT"]
        );
        assert_eq!(false, call_isolated! {f() where 1 => ">"}.res);
        assert_eq!(false, call_isolated! {f() where 1 => ">="}.res);
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
            &res.report.for_mutable(1).analysis.behavior.to_vec_ref(),
            &["EQ", "LT"]
        );
        let res = call_isolated! {f() where 1 => "<="};
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
        assert_eq!(call_isolated! {f(String::new()) where 2 => ">"}.res, false);
    }

    #[test]
    fn compare_tmp_vars() {
        #[muttest_codegen::mutate_isolated("binop_cmp")]
        fn f() -> bool {
            vec!["1", "2", "3"].into_iter().collect::<String>() < "123".to_owned()
        }

        assert_eq!(call_isolated! {f()}.res, false);
        assert_eq!(call_isolated! {f() where 1 => ">"}.res, false);
    }

    #[test]
    fn details_reported_before_covered() {
        #[muttest_codegen::mutate_isolated("binop_cmp")]
        #[allow(unreachable_code)]
        fn f() -> bool {
            1 < ({
                return false;
                2
            })
        }

        let res = call_isolated! {f()};
        assert_eq!(false, res.res);
        assert_ne!(res.report.for_mutable(1).location.module, None);
        assert_eq!(res.report.for_mutable(1).analysis.covered, false);
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
    }

    // TODO: tests for partialOrd
}
