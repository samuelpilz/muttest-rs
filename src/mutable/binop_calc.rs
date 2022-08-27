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
            core_crate,
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
                let left_type = #core_crate::phantom_for_type(&left);
                let right_type = #core_crate::phantom_for_type(&right);
                // this carries the output type of the computation
                // the assignment in the default-case defines the type of this phantom
                let mut output_type = ::core::marker::PhantomData;
                let mut_op = #core_crate::mutable::binop_calc::mutable_binop_calc(&#m_id, #loc);
                #[allow(unused_assignments)]
                match mut_op {
                    "" => {
                        let output = left #op right;
                        // after the computation is performed its output type is stored into the variable
                        // giving the compiler the necessary type hint required for the mutated cases
                        output_type = #core_crate::phantom_for_type(&output);
                        // report the possible mutations
                        #core_crate::report_possible_mutations(&#m_id,
                            &[
                                #((
                                    #op_symbols,
                                    {
                                        #[allow(unused_imports)]
                                        use #core_crate::mutable::binop_calc::#op_names::{IsNo, IsYes};
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
                            use #core_crate::mutable::binop_calc::#op_names::{IsNo, IsYes};
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
    report_coverage(m_id);
    match get_active_mutation_for_mutable(m_id).as_deref() {
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
            use core::marker::PhantomData;

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

    #[muttest_codegen::mutate_isolated("binop_calc")]
    fn mul_ints() -> i32 {
        5 * 4
    }

    #[test]
    fn mul_ints_mutables() {
        assert_eq!(mul_ints::NUM_MUTABLES, 1);
        assert_eq!(mul_ints::MUTABLES_CSV.lines().count(), 2);
    }

    #[test]
    fn mul_ints_unchanged() {
        assert_eq!(crate::tests::without_mutation(mul_ints), 20);
    }

    #[test]
    fn mul_ints_plus() {
        assert_eq!(crate::tests::with_mutation(1, "+", mul_ints), 9);
    }
    #[test]
    fn mul_ints_minus() {
        assert_eq!(crate::tests::with_mutation(1, "-", mul_ints), 1);
    }

    #[muttest_codegen::mutate_isolated("binop_calc")]
    fn calc_three_ints() -> i64 {
        3 + 2 * 4
    }

    #[test]
    fn calc_three_ints_mutables() {
        assert_eq!(calc_three_ints::NUM_MUTABLES, 2);
        assert_eq!(calc_three_ints::MUTABLES_CSV.lines().count(), 3);
        // TODO: assert that mutation 1 is `*` and mutation 2 is `-`
    }
    #[test]
    fn calc_three_ints_unchanged() {
        assert_eq!(crate::tests::without_mutation(calc_three_ints), 11);
    }

    #[test]
    fn calc_three_ints_1_minus() {
        assert_eq!(crate::tests::with_mutation(1, "-", calc_three_ints), 1);
    }

    #[test]
    fn calc_three_ints_2_div() {
        // tests implicit parentheses
        assert_eq!(crate::tests::with_mutation(2, "/", calc_three_ints), 0);
    }

    // TODO: speculative test requires inspection into details
    #[muttest_codegen::mutate_isolated("binop_calc")]
    fn s() -> String {
        "a".to_owned() + "b"
    }

    #[muttest_codegen::mutate_isolated("binop_calc")]
    fn a1() -> O1 {
        A + A
    }

    #[muttest_codegen::mutate_isolated("binop_calc")]
    fn a2() -> O2 {
        A - A
    }

    struct A;
    struct O1;
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

    #[test]
    fn tests() {
        crate::tests::without_mutation(|| {
            assert_eq!(&s(), "ab");
            assert!(matches!(a1(), O1));
            assert!(matches!(a2(), O2));
        });
    }
}
