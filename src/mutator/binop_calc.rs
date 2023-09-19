use std::ops::ControlFlow;

use quote::{format_ident, quote_spanned, ToTokens};
use syn::{spanned::Spanned, BinOp, Expr, ExprBinary};

use crate::transformer::{Mutated, MuttestTransformer, TransformSnippets};

use super::MutatorFns;

pub struct Mutator;

impl MutatorFns for Mutator {
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
        ) {
            return ControlFlow::Continue(());
        }

        let span = op.span();
        let op_str = op.to_token_stream().to_string();
        let TransformSnippets { m_id, muttest_api } = transformer.new_mutable(span);

        let (op_symbols, op_names) = CALC_OP_NAMES
            .iter()
            .filter(|o| o.0 != op_str)
            .map(|o| (o.0, format_ident!("{}", span = span, o.1)))
            .unzip::<_, _, Vec<_>, Vec<_>>();

        ControlFlow::Break(Mutated {
            transformed: quote_spanned! {span=>
                #muttest_api::id(#[allow(unused)]{
                    let ((left, left_type), (right, right_type)) =
                        (#muttest_api::typed(#left), #muttest_api::typed(#right));
                    let output_type = #muttest_api::PhantomData;
                    // dead branches to help type inference
                    if false {
                        #muttest_api::with_type(left #op right, output_type)
                    } else {
                        (#m_id).write_types(
                            "",
                            #muttest_api::PossibleMutations(&[
                                #((
                                    #op_symbols,
                                    {
                                        use #muttest_api::mutator::binop_calc::#op_names::IsOp;
                                        (&&(left_type, right_type, output_type)).is_op()
                                    }
                                ),)*
                            ])
                        );
                        (#m_id).write_coverage();
                        match (#m_id).get_action() {
                            #muttest_api::Option::None => left #op right,
                            #(#muttest_api::Option::Some(#op_symbols) =>
                                {
                                    use #muttest_api::mutator::binop_calc::#op_names::IsOp;
                                    (&&(left_type, right_type, output_type)).do_op(left, right)
                                }
                            )*
                            #muttest_api::Option::Some(m) => #muttest_api::internal_error(
                                #muttest_api::Error::InvalidMutation(m)
                            )
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
                use crate::{internal_error, Error};

                pub trait IsOp<L, R, O> {
                    fn is_op(&self) -> bool;
                    fn do_op(&self, left: L, right: R) -> O;
                }
                impl<L: ::std::ops::$t<R>, R> IsOp<L, R, <L as ::std::ops::$t<R>>::Output>
                    for &(
                        PhantomData<L>,
                        PhantomData<R>,
                        PhantomData<<L as ::std::ops::$t<R>>::Output>,
                    )
                {
                    fn is_op(&self) -> bool {
                        true
                    }
                    fn do_op(&self, left: L, right: R) -> <L as ::std::ops::$t<R>>::Output {
                        <L as ::std::ops::$t<R>>::$op(left, right)
                    }
                }
                impl<L, R, O> IsOp<L, R, O> for (PhantomData<L>, PhantomData<R>, PhantomData<O>) {
                    fn is_op(&self) -> bool {
                        false
                    }
                    fn do_op(&self, _: L, _: R) -> O {
                        internal_error(Error::InvalidSpecialization(module_path!()))
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
#[cfg_attr(feature = "selftest", muttest::tests)]
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
            CALC_OP_NAMES.len() - 1
        );
        assert_eq!(call_isolated! {f() where 1: "+"}.res, 9);
        assert_eq!(call_isolated! {f() where 1: "-"}.res, 1);
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
        assert_eq!(call_isolated! {f() where 1: "-"}.res, 1);
        // tests implicit parentheses
        assert_eq!(call_isolated! {f() where 2: "/"}.res, 0);
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
            res.report
                .for_mutable(1)
                .analysis
                .mutations
                .as_ref()
                .unwrap()
                .to_vec(),
            <Vec<String>>::new()
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
            res.report
                .for_mutable(1)
                .analysis
                .mutations
                .as_ref()
                .unwrap()
                .to_vec(),
            <Vec<String>>::new()
        );
        let res = call_isolated! {f2()};
        assert_eq!(res.res, O2);
        assert_eq!(
            res.report
                .for_mutable(1)
                .analysis
                .mutations
                .as_ref()
                .unwrap()
                .to_vec(),
            <Vec<String>>::new()
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
            res.report
                .for_mutable(1)
                .analysis
                .mutations
                .as_ref()
                .unwrap()
                .to_vec_ref(),
            vec!["-"]
        );
        assert!(now < res.res);

        let now = Instant::now();
        let res = call_isolated! {f(now) where 1: "-"};
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

        let res = call_isolated! {f(-2, 3) where 1: "/"};
        assert_eq!(res.res, 10);
        let res = call_isolated! {f(3, 2) where 2: "/"};
        assert_eq!(res.res, 10);
        let res = call_isolated! {f(-2, 2) where 3: "-"};
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
            &[">>"]
        );
        assert_eq!(res.res, 4);

        let res = call_isolated! {f() where 1: ">>"};
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

        let res = call_isolated! {f() where 1: "-"};
        assert_eq!(res.res, 2);
    }
}
