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
            BinOp::AddAssign(_)
                | BinOp::SubAssign(_)
                | BinOp::MulAssign(_)
                | BinOp::DivAssign(_)
                | BinOp::RemAssign(_)
                | BinOp::BitOrAssign(_)
                | BinOp::BitAndAssign(_)
                | BinOp::BitXorAssign(_)
                | BinOp::ShlAssign(_)
                | BinOp::ShrAssign(_)
        ) {
            return ControlFlow::Continue(());
        }

        let span = op.span();
        let op_str = op.to_token_stream().to_string();
        let TransformSnippets { m_id, muttest_api } = transformer.new_mutable(span);

        let (op_symbols, op_names) = CALC_ASSIGN_OP_NAMES
            .iter()
            .filter(|o| o.1 != op_str)
            .map(|o| (o.0, format_ident!("{}", span = span, o.1)))
            .unzip::<_, _, Vec<_>, Vec<_>>();

        ControlFlow::Break(Mutated {
            transformed: quote_spanned! {span=>
                #muttest_api::id(#[allow(unused)]{

                    // this mirrors the right-left order of evaluation for assign-ops
                    let (right, left) = (#right, &mut #left);

                    // these types carry the types involved in the calculation
                    // the assignment in the default-case defines the type of this phantom
                    let (left_type, right_type) =
                        (#muttest_api::PhantomData, #muttest_api::PhantomData);

                    // dead branches to help type inference
                    if false {
                        *#muttest_api::with_type(left, left_type)
                            #op #muttest_api::with_type(right, right_type);
                    } else {
                        (#m_id).write_types(
                            "",
                            #muttest_api::PossibleMutations(&[
                                #((
                                    #op_symbols,
                                    {
                                        use #muttest_api::mutator::binop_assign::#op_names::IsOp;
                                        (&&(left_type, right_type)).is_op()
                                    }
                                ),)*
                            ])
                        );
                        (#m_id).write_coverage();
                        match (#m_id).get_action() {
                            #muttest_api::Option::None => *left #op right,
                            #(#muttest_api::Option::Some(#op_symbols) =>
                                {
                                    use #muttest_api::mutator::binop_assign::#op_names::IsOp;
                                    (&&(left_type, right_type)).do_op(left, right)
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

macro_rules! binop_assign_traits {
    ($($op_code:literal, $op:ident, $t:ident;)*) => {
        const CALC_ASSIGN_OP_NAMES: &[(&str, &str)] = &[
            $(
                ($op_code, stringify!($op)),
            )*
        ];

        $(
            pub mod $op {
                use std::marker::PhantomData;
                use crate::{internal_error, Error};

                pub trait IsOp<L, R> {
                    fn is_op(&self) -> bool;
                    fn do_op(&self, left: &mut L, right: R);
                }
                impl<L: ::std::ops::$t<R>, R> IsOp<L, R> for &(PhantomData<&mut L>, PhantomData<R>)
                {
                    fn is_op(&self) -> bool {
                        true
                    }
                    fn do_op(&self, left: &mut L, right: R) {
                        <L as ::std::ops::$t<R>>::$op(left, right)
                    }
                }
                impl<L, R> IsOp<L, R> for (PhantomData<&mut L>, PhantomData<R>) {
                    fn is_op(&self) -> bool {
                        false
                    }
                    fn do_op(&self, _: &mut L, _: R) {
                        internal_error(Error::InvalidSpecialization(module_path!()))
                    }
                }
            }
        )*
    };
}

binop_assign_traits!(
    "+=", add_assign, AddAssign;
    "-=", sub_assign, SubAssign;
    "*=", mul_assign, MulAssign;
    "/=", div_assign, DivAssign;
    "%=", rem_assign, RemAssign;
    "&=", bitor_assign, BitOrAssign;
    "|=", bitand_assign, BitAndAssign;
    "^=", bitxor_assign, BitXorAssign;
    "<<=", shl_assign, ShlAssign;
    ">>=", shr_assign, ShrAssign;
);

#[cfg(test)]
#[cfg_attr(feature = "selftest", muttest::tests)]
mod tests {

    use super::CALC_ASSIGN_OP_NAMES;
    use crate::tests::*;

    #[test]
    fn mul_ints() {
        #[muttest_codegen::mutate_isolated("binop_assign")]
        fn f() -> i32 {
            let mut x = 5;
            x *= 4;
            x
        }
        let data = data_isolated!(f);
        assert_eq!(data.mutables.len(), 1);

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
            CALC_ASSIGN_OP_NAMES.len()
        );
        assert_eq!(call_isolated! {f() where 1: "+="}.res, 9);
        assert_eq!(call_isolated! {f() where 1: "-="}.res, 1);
    }

    #[test]
    fn add_str() {
        #[muttest_codegen::mutate_isolated("binop_assign")]
        fn f(mut s: String) -> String {
            s += "b";
            s
        }
        let res = call_isolated! {f("a".to_owned())};
        assert_eq!(&*res.res, "ab");
        assert_eq!(
            &res.report
                .for_mutable(1)
                .analysis
                .mutations
                .as_ref()
                .unwrap()
                .to_vec_ref(),
            &["+="]
        );
    }

    #[test]
    fn xor_assign_on_array_elems() {
        #[muttest_codegen::mutate_isolated("binop_assign")]
        fn f(s: &mut [u8]) {
            s[0] ^= s[1];
        }
        let data = data_isolated!(f);
        assert_eq!(data.mutables.len(), 1);

        let mut x = [1, 3];
        let res = call_isolated! {f(&mut x)};
        assert_eq!(x, [2, 3]);
        assert_eq!(
            res.report
                .for_mutable(1)
                .analysis
                .mutations
                .as_ref()
                .unwrap()
                .len(),
            // all mutations possible
            CALC_ASSIGN_OP_NAMES.len()
        );

        let mut x = [1, 3];
        call_isolated! {f(&mut x) where 1: "+="};
        assert_eq!(x, [4, 3]);
    }
}
