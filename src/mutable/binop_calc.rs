use proc_macro2::{Span, TokenStream};
use quote::{format_ident, quote_spanned, ToTokens};
use syn::{spanned::Spanned, BinOp, Expr, ExprBinary};

use crate::transformer::{strip_expr_parens, MuttestTransformer, TransformSnippets};

use super::MatchMutable;

pub struct Mutable<'a> {
    pub left: &'a dyn ToTokens,
    pub right: &'a dyn ToTokens,
    pub op: &'a dyn ToTokens,
    pub span: Span,
}

impl<'a> MatchMutable<'a, Expr> for Mutable<'a> {
    fn try_match(expr: &'a Expr) -> Option<Self> {
        match expr {
            Expr::Binary(ExprBinary {
                left, op, right, ..
            }) if matches!(
                op,
                BinOp::Add(_)
                    | BinOp::Sub(_)
                    | BinOp::Mul(_)
                    | BinOp::Div(_)
                    | BinOp::Rem(_)
                    | BinOp::BitOr(_)
                    | BinOp::BitAnd(_)
                    | BinOp::BitXor(_)
                    | BinOp::Shl(_)
                    | BinOp::Shr(_)
            ) =>
            {
                Some(Self {
                    left: strip_expr_parens(left),
                    right: strip_expr_parens(right),
                    op,
                    span: op.span(),
                })
            }
            _ => None,
        }
    }
}

impl<'a> super::Mutable<'a> for Mutable<'a> {
    const NAME: &'static str = "binop_calc";

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

        let mutations = CALC_OP_NAMES;
        // TODO: remove current op from list
        let op_symbols = mutations.iter().map(|x| x.0).collect::<Vec<_>>();
        let op_names = mutations
            .iter()
            .map(|x| format_ident!("{}", span = span, x.1))
            .collect::<Vec<_>>();

        quote_spanned! {span=>
            #muttest_api::id({
                // these types carry the types involved in the calculation
                // the assignment in the default-case defines the type of this phantom
                #[allow(unused_mut)]
                let (mut left_type, mut right_type, mut output_type) =
                    (#muttest_api::PhantomData, #muttest_api::PhantomData,#muttest_api::PhantomData);

                let __muttest_mutation = (#m_id).get_active_mutation();

                // TODO: this has exponential blowup of code-size. Dead branches should use original code instead
                // dead branches to help type inference
                #[allow(unused_assignments)]
                match 0 {
                    _ if __muttest_mutation.is_skip() => {
                        let (_left, _right) = (#left, #right);
                        left_type = #muttest_api::phantom_for_type(&_left);
                        right_type = #muttest_api::phantom_for_type(&_right);
                        let _output = _left #op _right;
                        output_type = #muttest_api::phantom_for_type(&_output);
                        _output
                    }
                    _ => {
                        __muttest_mutation.report_details(
                            #loc,
                            "",
                            &#muttest_api::mutation_string_from_bool_list(&[
                                #((
                                    #op_symbols,
                                    {
                                        #[allow(unused_imports)]
                                        use #muttest_api::mutable::binop_calc::#op_names::{IsNo, IsYes};
                                        (&(left_type, right_type, output_type))
                                            .get_impl()
                                            .is_impl()
                                    }
                                ),)*
                            ])
                        );
                        let (_left, _right) = (#left, #right);
                        __muttest_mutation.report_coverage(#muttest_api::Option::None);
                        match &*__muttest_mutation.as_option().unwrap_or_default() {
                            "" => _left #op _right,
                            #(#op_symbols =>
                                {
                                    #[allow(unused_imports)]
                                    use #muttest_api::mutable::binop_calc::#op_names::{IsNo, IsYes};
                                    (&(left_type, right_type, output_type))
                                        .get_impl()
                                        .run(_left, _right)
                                }
                            )*
                            _ => todo!()
                        }
                    }
                }
            })
        }
    }
}

macro_rules! binop_calc_traits {
    ($($op_code:literal, $op:ident, $t:ident;)*) => {
        const CALC_OP_NAMES: &[(&str, &str)] = &[
            $(
                ($op_code, stringify!($op)),
            )*
        ];

        $(
            pub mod $op {
                use std::marker::PhantomData;

                pub struct Yes;
                pub struct No;

                pub trait IsYes {
                    fn get_impl(&self) -> Yes;
                }
                pub trait IsNo {
                    fn get_impl(&self) -> No;
                }
                impl<L: ::std::ops::$t<R>, R> IsYes
                    for (
                        PhantomData<L>,
                        PhantomData<R>,
                        PhantomData<<L as ::std::ops::$t<R>>::Output>,
                    )
                {
                    fn get_impl(&self) -> Yes {
                        Yes
                    }
                }
                impl<L, R, O> IsNo for &(PhantomData<L>, PhantomData<R>, PhantomData<O>) {
                    fn get_impl(&self) -> No {
                        No
                    }
                }
                impl Yes {
                    pub fn is_impl(&self) -> bool {
                        true
                    }
                    pub fn run<L: ::std::ops::$t<R>, R>(
                        self,
                        left: L,
                        right: R,
                    ) -> <L as ::std::ops::$t<R>>::Output {
                        <L as ::std::ops::$t<R>>::$op(left, right)
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
            }
        )*
    };
}

binop_calc_traits!(
    "+", add, Add;
    "-", sub, Sub;
    "*", mul, Mul;
    "/", div, Div;
    "%", rem, Rem;
    "&", bitor, BitOr;
    "|", bitand, BitAnd;
    "^", bitxor, BitXor;
    "<<", shl, Shl;
    ">>", shr, Shr;
);

#[cfg(test)]
mod tests {
    use std::{
        ops::{Add, Sub},
        time::{Duration, Instant},
    };

    use super::CALC_OP_NAMES;
    use crate::tests::*;

    #[test]
    fn mul_ints() {
        #[muttest_codegen::mutate_isolated("binop_calc")]
        fn f() -> i32 {
            5 * 4
        }
        let report = data_isolated!(f);
        assert_eq!(report.mutables.len(), 1);

        let res = call_isolated! {f()};
        assert_eq!(res.res, 20);
        assert_eq!(
            res.report
                .for_mutable(1)
                .analysis
                .mutations
                .as_ref()
                .unwrap()
                .len(),
            // all mutations possible
            CALC_OP_NAMES.len()
        );
        assert_eq!(call_isolated! {f() where 1 => "+"}.res, 9);
        assert_eq!(call_isolated! {f() where 1 => "-"}.res, 1);
    }

    #[test]
    fn calc_three_ints() {
        #[muttest_codegen::mutate_isolated("binop_calc")]
        fn f() -> i64 {
            3 + 2 * 4
        }

        let report = data_isolated!(f);
        assert_eq!(report.mutables.len(), 2);
        assert_eq!(&report.for_mutable(1).analysis.code, "*");
        assert_eq!(&report.for_mutable(2).analysis.code, "+");

        assert_eq!(call_isolated! {f()}.res, 11);
        assert_eq!(call_isolated! {f() where 1 => "-"}.res, 1);
        // tests implicit parentheses
        assert_eq!(call_isolated! {f() where 2 => "/"}.res, 0);
    }

    #[test]
    fn add_of_strings() {
        #[muttest_codegen::mutate_isolated("binop_calc")]
        fn f() -> String {
            "a".to_owned() + "b"
        }

        let res = call_isolated! {f()};
        assert_eq!(&*res.res, "ab");
        assert_eq!(
            &res.report
                .for_mutable(1)
                .analysis
                .mutations
                .as_ref()
                .unwrap()
                .to_vec_ref(),
            &["+"]
        );
    }

    #[test]
    fn add_with_different_sub() {
        #[muttest_codegen::mutate_isolated("binop_calc")]
        fn f1() -> O1 {
            A + A
        }

        #[muttest_codegen::mutate_isolated("binop_calc")]
        fn f2() -> O2 {
            A - A
        }

        struct A;
        #[derive(Debug, PartialEq, Eq)]
        struct O1;
        #[derive(Debug, PartialEq, Eq)]
        struct O2;

        impl Add for A {
            type Output = O1;

            fn add(self, _: Self) -> Self::Output {
                O1
            }
        }
        impl Sub for A {
            type Output = O2;

            fn sub(self, _: Self) -> Self::Output {
                O2
            }
        }

        let res = call_isolated! {f1()};
        assert_eq!(res.res, O1);
        assert_eq!(
            &res.report
                .for_mutable(1)
                .analysis
                .mutations
                .as_ref()
                .unwrap()
                .to_vec_ref(),
            &["+"]
        );
        let res = call_isolated! {f2()};
        assert_eq!(res.res, O2);
        assert_eq!(
            &res.report
                .for_mutable(1)
                .analysis
                .mutations
                .as_ref()
                .unwrap()
                .to_vec_ref(),
            &["-"]
        );
    }

    #[test]
    fn calc_with_time() {
        #[muttest_codegen::mutate_isolated("binop_calc")]
        fn f(i: Instant) -> Instant {
            i + Duration::new(1, 0)
        }

        let now = Instant::now();
        let res = call_isolated! {f(now)};
        assert_eq!(
            &res.report
                .for_mutable(1)
                .analysis
                .mutations
                .as_ref()
                .unwrap()
                .to_vec_ref(),
            &["+", "-"]
        );
        assert!(now < res.res);

        let now = Instant::now();
        let res = call_isolated! {f(now) where 1 => "-"};
        assert!(now > res.res);
    }

    #[test]
    fn sum_of_squares() {
        // this test makes sure that the order of operation is intact
        #[muttest_codegen::mutate_isolated("binop_calc")]
        fn f(x: i8, y: i8) -> i8 {
            x * x + y * y
        }

        let report = data_isolated!(f);
        assert_eq!(&report.for_mutable(1).analysis.code, "*");
        assert_eq!(&report.for_mutable(2).analysis.code, "*");
        assert_eq!(&report.for_mutable(3).analysis.code, "+");

        let res = call_isolated! {f(-4, 7)};
        assert_eq!(res.res, 16 + 49);

        let res = call_isolated! {f(-2, 3) where 1 => "/"};
        assert_eq!(res.res, 10);
        let res = call_isolated! {f(3, 2) where 2 => "/"};
        assert_eq!(res.res, 10);
        let res = call_isolated! {f(-2, 2) where 3 => "-"};
        assert_eq!(res.res, 0);
    }

    #[test]
    fn shift_different_rhs_type() {
        #[muttest_codegen::mutate_isolated("binop_calc")]
        fn f() -> u8 {
            1 << 2i32
        }

        let res = call_isolated! {f()};
        assert_eq!(
            &res.report
                .for_mutable(1)
                .analysis
                .mutations
                .as_ref()
                .unwrap()
                .to_vec_ref(),
            &["<<", ">>"]
        );
        assert_eq!(res.res, 4);

        let res = call_isolated! {f() where 1 => ">>"};
        assert_eq!(res.res, 0);
    }

    #[test]
    fn consts_not_mutated() {
        #[muttest_codegen::mutate_isolated("binop_calc")]
        fn _f() {
            const X1: u32 = 1 + 1;
            static X2: u8 = 1 - 1;
            const fn plus1(x: u8) -> u8 {
                x + 1
            }
        }
        let data = data_isolated!(_f);
        assert_eq!(data.mutables.len(), 0);
    }
    #[test]
    fn const_generics_not_mutated() {
        #[muttest_codegen::mutate_isolated("binop_calc")]
        fn _f() -> [(); 4 + 5] {
            [(); 4 + 5]
        }
        let data = data_isolated!(_f);
        assert_eq!(data.mutables.len(), 0);
    }
    #[test]
    fn enum_discriminant_not_mutated() {
        #[muttest_codegen::mutate_isolated("binop_calc")]
        fn _f() {
            enum X {
                A = 1 + 2,
                B = 2,
            }
        }
        let report = data_isolated!(_f);
        assert_eq!(report.mutables.len(), 0);
    }
    #[test]
    fn pattern_guard_mutated() {
        #[muttest_codegen::mutate_isolated("binop_calc")]
        fn f() -> usize {
            match 0 {
                _ if 1 + 1 == 2 => 1,
                _ => 2,
            }
        }
        let res = call_isolated! {f()};
        assert_eq!(res.res, 1);

        let res = call_isolated! {f() where 1 => "-"};
        assert_eq!(res.res, 2);
    }

    #[test]
    fn details_reported_before_covered() {
        #[muttest_codegen::mutate_isolated("binop_calc")]
        #[allow(unreachable_code)]
        fn f() -> u8 {
            ({
                return 5;
                1
            }) / 3
        }

        let res = call_isolated! {f()};
        assert_eq!(5, res.res);
        assert_ne!(res.report.for_mutable(1).analysis.mutations, None);
        assert_eq!(res.report.for_mutable(1).analysis.covered, false);
    }
}
