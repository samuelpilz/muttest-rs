use std::ops::ControlFlow;

use quote::{format_ident, quote_spanned, ToTokens};
use syn::{spanned::Spanned, Expr, ExprUnary, UnOp};

use crate::transformer::{Mutated, MuttestTransformer, TransformSnippets};

use super::MutatorFns;

pub struct Mutator;

impl MutatorFns for Mutator {
    fn transform_expr(
        &self,
        transformer: &mut MuttestTransformer,
        e: &Expr,
    ) -> ControlFlow<Mutated> {
        let Expr::Unary(ExprUnary { op, expr, .. }) = e else {
            return ControlFlow::Continue(());
        };
        if !matches!(op, UnOp::Not(_) | UnOp::Neg(_)) {
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
                    let (expr, expr_type) = #muttest_api::typed(#expr);
                    let output_type = #muttest_api::PhantomData;
                    // dead branches to help type inference
                    if false {
                        #muttest_api::with_type(#op expr, output_type)
                    } else {
                        (#m_id).write_types(
                            "",
                            #muttest_api::PossibleMutations(&[
                                #((
                                    #op_symbols,
                                    {
                                        use #muttest_api::mutator::unop_calc::#op_names::IsOp;
                                        (&&(expr_type, output_type)).is_op()
                                    }
                                ),)*
                            ])
                        );
                        (#m_id).write_coverage();
                        match (#m_id).get_action() {
                            #muttest_api::Option::None => #op expr,
                            #(#muttest_api::Option::Some(#op_symbols) =>
                                {
                                    use #muttest_api::mutator::unop_calc::#op_names::IsOp;
                                    (&&(expr_type, output_type)).do_op(expr)
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

macro_rules! unop_calc_traits {
    ($($op_code:literal, $op:ident, $t:path;)*) => {
        const CALC_OP_NAMES: &[(&str, &str)] = &[
            $(
                ($op_code, stringify!($op)),
            )*
        ];

        $(
            pub mod $op {
                use std::marker::PhantomData;
                use crate::{internal_error, Error};

                pub trait IsOp<T, O> {
                    fn is_op(&self) -> bool;
                    fn do_op(&self, t: T) -> O;
                }
                impl<T: $t> IsOp<T, <T as $t>::Output>
                    for &(
                        PhantomData<T>,
                        PhantomData<<T as $t>::Output>,
                    )
                {
                    fn is_op(&self) -> bool {
                        true
                    }
                    fn do_op(&self, t: T) -> <T as $t>::Output {
                        <T as $t>::$op(t)
                    }
                }
                impl<T, O> IsOp<T, O> for (PhantomData<T>, PhantomData<O>) {
                    fn is_op(&self) -> bool {
                        false
                    }
                    fn do_op(&self, _: T) -> O {
                        internal_error(Error::InvalidSpecialization(module_path!()))
                    }
                }
            }
        )*
    };
}
// trait in the style of noop
pub trait NoOp: Sized {
    type Output;
    fn no_op(self) -> Self::Output;
}
impl<T> NoOp for T {
    type Output = Self;
    fn no_op(self) -> Self::Output {
        self
    }
}

unop_calc_traits!(
    "-", neg, ::std::ops::Neg;
    "!", not, ::std::ops::Not;
    "no-op", no_op, super::NoOp;
);

#[cfg(test)]
#[cfg_attr(feature = "selftest", muttest::tests)]
mod tests {

    use crate::tests::*;

    #[test]
    fn negate_int() {
        #[muttest_codegen::mutate_isolated("unop_calc")]
        fn f() -> i32 {
            -1
        }
        let report = data_isolated!(f);
        assert_eq!(report.mutables.len(), 1);

        let res = call_isolated! {f()};
        assert_eq!(res.res, -1);
        assert_eq!(
            res.report
                .for_mutable(1)
                .analysis
                .mutations
                .as_ref()
                .unwrap()
                .to_vec_deref()
                .sorted(),
            vec!["!", "no-op"]
        );
        assert_eq!(call_isolated! {f() where 1: "!"}.res, -2);
        assert_eq!(call_isolated! {f() where 1: "no-op"}.res, 1);
    }

    #[test]
    fn not_bool() {
        #[muttest_codegen::mutate_isolated("unop_calc")]
        fn f() -> bool {
            !false
        }

        let res = call_isolated! {f()};
        assert_eq!(res.res, true);
        assert_eq!(
            res.report
                .for_mutable(1)
                .analysis
                .mutations
                .as_ref()
                .unwrap()
                .to_vec_deref()
                .sorted(),
            vec!["no-op"]
        );

        assert_eq!(call_isolated! {f() where 1: "no-op"}.res, false);
    }

    // TODO: more tests
}
