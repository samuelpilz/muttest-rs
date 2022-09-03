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
                // these types carry the types involved in the calculation
                // the assignment in the default-case defines the type of this phantom
                let mut left_type = #muttest_api::PhantomData;
                let mut right_type = #muttest_api::PhantomData;
                let mut output_type = #muttest_api::PhantomData;

                // TODO: this has exponential blowup of code-size. Dead branches should use original code instead
                // dead branches to help type inference
                #[allow(unused_assignments)]
                match 0 {
                    1 => #left #op #right,
                    // type-check almost-original code
                    2 => {
                        let (left, right) = (#left, #right);
                        left_type = #muttest_api::phantom_for_type(&left);
                        right_type = #muttest_api::phantom_for_type(&right);
                        let output = left #op right;
                        output_type = #muttest_api::phantom_for_type(&output);
                        output
                    }
                    _ => {
                        (#m_id).report_details(
                            #loc,
                            vec![
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
                        let (left, right) = (#left, #right);
                        match &*#muttest_api::mutable::binop_calc::run(&#m_id) {
                            "" => left #op right,
                            #(#op_symbols =>
                                {
                                    #[allow(unused_imports)]
                                    use #muttest_api::mutable::binop_calc::#op_names::{IsNo, IsYes};
                                    (&(left_type, right_type, output_type))
                                        .get_impl()
                                        .run(left, right)
                                }
                            )*
                            _ => todo!()
                        }
                    }
                }
            },).0
        }
    }
}

pub fn run(m_id: &MutableId<'static>) -> String {
    m_id.get_active_mutation().unwrap_or_default()
}

macro_rules! binop_calc_traits {
    ($m:ident, $t:ident, $f:ident) => {
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
                    <L as ::std::ops::$t<R>>::$f(left, right)
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

binop_calc_traits!(add, Add, add);
binop_calc_traits!(sub, Sub, sub);
binop_calc_traits!(mul, Mul, mul);
binop_calc_traits!(div, Div, div);
binop_calc_traits!(rem, Rem, rem);

#[cfg(test)]
mod tests {
    use std::{
        ops::{Add, Sub},
        time::{Duration, Instant},
    };

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
            Some(&vec!["+".to_owned()])
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
            res.data
                .mutables
                .get(&mutable_id(1))
                .and_then(|x| x.possible_mutations.as_ref()),
            Some(&vec!["+".to_owned()])
        );
        let res = call_isolated! {f2()};
        assert_eq!(res.res, O2);
        assert_eq!(
            res.data
                .mutables
                .get(&mutable_id(1))
                .and_then(|x| x.possible_mutations.as_ref()),
            Some(&vec!["-".to_owned()])
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
            res.data
                .mutables
                .get(&mutable_id(1))
                .and_then(|x| x.possible_mutations.as_ref()),
            Some(&vec!["+".to_owned(), "-".to_owned()])
        );
        assert!(now < res.res);

        let now = Instant::now();
        let res = call_isolated! {f(now) where 1 => "-"};
        assert!(now > res.res);
    }
}

// TODO: test x*x + y*y
// TODO: test that details are reported, even if left&right fail
