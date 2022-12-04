use proc_macro2::{Span, TokenStream};
use quote::{format_ident, quote_spanned, ToTokens};
use syn::{spanned::Spanned, Expr, ExprAssignOp};

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
            Expr::AssignOp(ExprAssignOp {
                left, op, right, ..
            }) => Some(Self {
                left: strip_expr_parens(left),
                right: strip_expr_parens(right),
                op,
                span: op.span(),
            }),
            _ => None,
        }
    }
}

impl<'a> super::Mutable<'a> for Mutable<'a> {
    const NAME: &'static str = "assign_op";

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

        let mutations = CALC_ASSIGN_OP_NAMES;
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
                let (mut left_type, mut right_type) =
                    (#muttest_api::PhantomData, #muttest_api::PhantomData);

                let __muttest_mutation = (#m_id).get_active_mutation();

                // TODO: this has exponential blowup of code-size. Dead branches should use original code instead
                // dead branches to help type inference
                #[allow(unused_assignments)]
                match 0 {
                    _ if __muttest_mutation.is_skip() => {
                        let (mut _left, _right) = (&mut (#left), #right);
                        left_type = #muttest_api::phantom_for_type(&_left);
                        right_type = #muttest_api::phantom_for_type(&_right);
                        *_left #op _right;
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
                                        use #muttest_api::mutable::assign_op::#op_names::{IsNo, IsYes};
                                        (&(left_type, right_type))
                                            .get_impl()
                                            .is_impl()
                                    }
                                ),)*
                            ])
                        );
                        let (mut _left, _right) = (&mut #left, #right);
                        __muttest_mutation.report_coverage(#muttest_api::Option::None);
                        match __muttest_mutation.as_option() {
                            #muttest_api::Option::None => *_left #op _right,
                            #(#muttest_api::Option::Some(#op_symbols) =>
                                {
                                    #[allow(unused_imports)]
                                    use #muttest_api::mutable::assign_op::#op_names::{IsNo, IsYes};
                                    (&(left_type, right_type))
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

macro_rules! assign_op_traits {
    ($($op_code:literal, $op:ident, $t:ident;)*) => {
        const CALC_ASSIGN_OP_NAMES: &[(&str, &str)] = &[
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
                impl<L: ::std::ops::$t<R>, R> IsYes for (PhantomData<&mut L>, PhantomData<R>)
                {
                    fn get_impl(&self) -> Yes {
                        Yes
                    }
                }
                impl<L, R> IsNo for &(PhantomData<&mut L>, PhantomData<R>) {
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
                        left: &mut L,
                        right: R,
                    ) {
                        <L as ::std::ops::$t<R>>::$op(left, right)
                    }
                }
                impl No {
                    pub fn is_impl(&self) -> bool {
                        false
                    }
                    pub fn run<L, R>(self, _: &mut L, _: R) {
                        unreachable!()
                    }
                }
            }
        )*
    };
}

assign_op_traits!(
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
mod tests {

    use super::CALC_ASSIGN_OP_NAMES;
    use crate::tests::*;

    #[test]
    fn mul_ints() {
        #[muttest_codegen::mutate_isolated("assign_op")]
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
        assert_eq!(call_isolated! {f() where 1 => "+="}.res, 9);
        assert_eq!(call_isolated! {f() where 1 => "-="}.res, 1);
    }

    #[test]
    fn add_str() {
        #[muttest_codegen::mutate_isolated("assign_op")]
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

    // TODO: tests
}
