use proc_macro2::{Span, TokenStream};
use quote::{format_ident, quote_spanned, ToTokens};

use crate::{
    transformer::{Mutable, MuttestTransformer, TransformSnippets},
    *,
};

pub struct MutableBinopCalc<'a> {
    pub left: &'a dyn ToTokens,
    pub right: &'a dyn ToTokens,
    pub op: &'a dyn ToTokens,
    pub span: Span,
}

impl<'a> Mutable<'a> for MutableBinopCalc<'a> {
    const NAME: &'static str = "binop_calc";

    fn transform(self, transformer: &mut MuttestTransformer) -> TokenStream {
        let span = self.span;
        let op = self.op.to_token_stream();
        let op_str = op.to_string();
        let (left, right) = (self.left, self.right);

        let TransformSnippets {
            m_id,
            muttest_api,
            loc,
        } = transformer.new_mutable::<Self>(&op_str, span);

        let mutations = [
            ("+", "add"),
            ("-", "sub"),
            ("*", "mul"),
            ("/", "div"),
            ("%", "rem"),
        ];
        let op_symbols = mutations.map(|x| x.0);
        let op_names = mutations.map(|x| format_ident!("{}", span = span, x.1));

        quote_spanned! {span=>
            ({
                // arguments are evaluated before executing the calculation
                let (left, right) = (#left, #right);
                let left_type = #muttest_api::phantom_for_type(&left);
                let right_type = #muttest_api::phantom_for_type(&right);
                // this carries the output type of the computation
                // the assignment in the default-case defines the type of this phantom
                let mut output_type = #muttest_api::PhantomData;
                let mut_op = #muttest_api::mutable::binop_calc::mutable_binop_calc(&#m_id, #loc);
                #[allow(unused_assignments)]
                match mut_op {
                    "" => {
                        let output = left #op right;
                        // after the computation is performed its output type is stored into the variable
                        // giving the compiler the necessary type hint required for the mutated cases
                        output_type = #muttest_api::phantom_for_type(&output);
                        // report the possible mutations
                        // TODO: this is only called if original code returns (maybe add reporter for mutable-termination?)
                        (#m_id).report_possible_mutations(
                            &[
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
                            ]
                        );
                        output
                    },
                    // possible mutations
                    #(#op_symbols =>
                        {
                            #[allow(unused_imports)]
                            use #muttest_api::mutable::binop_calc::#op_names::{IsNo, IsYes};
                            (&(left_type, right_type, output_type))
                                .get_impl()
                                .run(left, right)
                        }
                    )*
                    // TODO: report error and panic
                    _ => unreachable!(),
                }
            },).0
        }
    }
}

pub fn mutable_binop_calc(m_id: &MutableId<'static>, loc: MutableLocation) -> &'static str {
    m_id.report_at(loc);

    match m_id.get_active_mutation().as_deref() {
        None => "",
        Some("-") => "-",
        Some("+") => "+",
        Some("*") => "*",
        Some("/") => "/",
        Some("%") => "%",
        _ => todo!(),
    }
}

macro_rules! binop_calc_traits {
    ($m:ident, $t:path, $f:ident) => {
        pub mod $m {
            use std::marker::PhantomData;

            pub struct Yes;
            pub struct No;

            pub trait IsYes {
                fn get_impl(&self) -> Yes;
            }
            pub trait IsNo {
                fn get_impl(&self) -> No;
            }
            impl<L: $t, R> IsYes
                for (
                    PhantomData<L>,
                    PhantomData<R>,
                    PhantomData<<L as $t>::Output>,
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
                pub fn run<L: $t, R>(self, left: L, right: R) -> <L as $t>::Output {
                    <L as $t>::$f(left, right)
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
    };
}

binop_calc_traits!(add, std::ops::Add<R>, add);
binop_calc_traits!(sub, std::ops::Sub<R>, sub);
binop_calc_traits!(mul, std::ops::Mul<R>, mul);
binop_calc_traits!(div, std::ops::Div<R>, div);
binop_calc_traits!(rem, std::ops::Rem<R>, rem);

#[cfg(test)]
mod tests {
    use std::ops::{Add, Sub};

    use crate::tests::*;

    #[test]
    fn mul_ints() {
        #[muttest_codegen::mutate_isolated("binop_calc")]
        fn f() -> i32 {
            5 * 4
        }
        let data = data_isolated!(f);
        assert_eq!(data.mutables.len(), 1);

        assert_eq!(call_isolated! {f()}.res, 20);
        assert_eq!(call_isolated! {f() where 1 => "+"}.res, 9);
        assert_eq!(call_isolated! {f() where 1 => "-"}.res, 1);
    }

    #[test]
    fn calc_three_ints() {
        #[muttest_codegen::mutate_isolated("binop_calc")]
        fn f() -> i64 {
            3 + 2 * 4
        }

        let data = data_isolated!(f);
        assert_eq!(data.mutables.len(), 2);
        // TODO: assert that mutation 1 is `*` and mutation 2 is `-`

        assert_eq!(call_isolated! {f()}.res, 11);
        assert_eq!(call_isolated! {f() where 1 => "-"}.res, 1);
        // tests implicit parentheses
        assert_eq!(call_isolated! {f() where 2 => "/"}.res, 0);
    }

    #[test]
    #[ignore]
    fn add_of_strings() {
        #[muttest_codegen::mutate_isolated("binop_calc")]
        fn f() -> String {
            "a".to_owned() + "b"
        }

        let res = call_isolated! {f()};
        assert_eq!(&*res.res, "ab");
        assert_eq!(
            res.data
                .mutables
                .get(&mutable_id(1))
                .and_then(|x| x.possible_mutations.as_ref()),
            Some(&vec![])
        );
    }

    #[test]
    #[ignore]
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
            res.data
                .mutables
                .get(&mutable_id(1))
                .and_then(|x| x.possible_mutations.as_ref()),
            Some(&vec![])
        );
        let res = call_isolated! {f2()};
        assert_eq!(res.res, O2);
        assert_eq!(
            res.data
                .mutables
                .get(&mutable_id(1))
                .and_then(|x| x.possible_mutations.as_ref()),
            Some(&vec![])
        );
    }
}
