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

pub use crate::get_binop_calc;

#[macro_export]
macro_rules! get_binop_calc {
    ($op_mod:path, $left:expr, $right:expr, $o:expr) => {{
        #[allow(unused_imports)]
        use $op_mod::{IsNo, IsYes};
        (&($left, $right, $o)).get_impl()
    }};
}
