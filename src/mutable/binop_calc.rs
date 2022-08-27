use crate::*;

pub fn mutable_binop_calc(m_id: &MutableId, _op_str: &'static str) -> &'static str {
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

    #[muttest_codegen::mutate_isolated("binop-calc")]
    fn s() -> String {
        "a".to_owned() + "b"
    }

    #[muttest_codegen::mutate_isolated("binop-calc")]
    fn ints() -> i32 {
        5 * 4
    }

    #[muttest_codegen::mutate_isolated("binop-calc")]
    fn a1() -> O1 {
        A + A
    }

    #[muttest_codegen::mutate_isolated("binop-calc")]
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
            assert_eq!(ints(), 20);
        });
    }
}
