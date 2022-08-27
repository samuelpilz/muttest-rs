use std::{
    marker::PhantomData,
};

use super::*;

mod lit_int;
mod lit_str;

pub use lit_str::*;
pub use lit_int::*;

pub fn mutable_cmp<T: PartialOrd<T1>, T1>(
    m_id: &MutableId,
    op_str: &str,
    left: &T,
    right: &T1,
) -> bool {
    report_coverage(m_id);
    let ord = left.partial_cmp(right);
    // TODO: record behavior for weak mutation testing
    // save_msg(&format!("CMP {m_id}; {ord:?}"));
    if let Some(ord) = ord {
        match get_active_mutation_for_mutable(m_id)
            .as_deref()
            .unwrap_or(op_str)
        {
            "<" => ord.is_lt(),
            "<=" => ord.is_le(),
            ">=" => ord.is_ge(),
            ">" => ord.is_gt(),
            _ => todo!(),
        }
    } else {
        false
    }
}

pub fn mutable_bin_op(m_id: &MutableId, _op_str: &'static str) -> &'static str {
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

pub fn phantom_for_type<T>(_: &T) -> PhantomData<T> {
    PhantomData
}

macro_rules! binop_mutation {
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

binop_mutation!(add, std::ops::Add<R>, add);
binop_mutation!(sub, std::ops::Sub<R>, sub);
binop_mutation!(mul, std::ops::Mul<R>, mul);
binop_mutation!(div, std::ops::Div<R>, div);
binop_mutation!(rem, std::ops::Rem<R>, rem);

#[macro_export]
macro_rules! get_binop {
    ($op_mod:path, $left:expr, $right:expr, $o:expr) => {{
        #[allow(unused_imports)]
        use $op_mod::{IsNo, IsYes};
        (&($left, $right, $o)).get_impl()
    }};
}
