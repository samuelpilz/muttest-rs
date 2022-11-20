use std::{marker::PhantomData, ops::ControlFlow};

use proc_macro2::{Span, TokenStream};
use quote::{quote_spanned, ToTokens};

use crate::{
    transformer::{MuttestTransformer, TransformSnippets},
    BakedMutableId,
};

// TODO: mutate (some) blocks instead of `ItemFn`s
pub struct Mutable<'a> {
    pub vis: &'a dyn ToTokens,
    pub sig: &'a dyn ToTokens,
    pub block: &'a dyn ToTokens,
    pub span: Span,
}

impl<'a> super::Mutable<'a> for Mutable<'a> {
    const NAME: &'static str = "extreme";

    fn span(&self) -> Span {
        self.span
    }

    fn transform(self, transformer: &mut MuttestTransformer) -> TokenStream {
        let span = self.span;
        let TransformSnippets {
            m_id,
            muttest_api,
            loc,
        } = transformer.new_mutable(&self, "");
        // TODO: add reasonable code for that

        let Self {
            vis, sig, block, ..
        } = self;
        quote_spanned! {span=>
            #vis #sig {
                let ret_type = #muttest_api::PhantomData;

                // println!("RUN-extreme");
                let mut muttest_nesting_token = crate::tests::NestingToken::create(#m_id);
                // if matches!(xyz, crate::tests::NestingToken::Nested) {
                //     println!("RUN-extreme NESTED");
                // }

                // dead branches help type inference
                match 0 {
                    // type-check the original code first
                    _ if muttest_nesting_token.is_nested() => #block,
                    // unify the ret_type with expression-type of block
                    2 => #muttest_api::mutable::extreme::phantom_unwrap(ret_type),
                    // unify the ret_type with fn return value
                    3 => return #muttest_api::mutable::extreme::phantom_unwrap(ret_type),

                    // run mutable
                    _ => {
                        // report static analysis
                        (#m_id).report_details(
                            #loc,
                            "", // TODO: try print the type
                            &#muttest_api::mutation_string_from_bool_list(
                                &[
                                    ("default",
                                    {
                                        #[allow(unused_imports)]
                                        use #muttest_api::mutable::extreme::{NotDefault, YesDefault};
                                        (&ret_type).is_default()
                                    }),
                                    ("panic", true),
                                ]
                            )
                        );

                        match #muttest_api::mutable::extreme::run(#m_id,
                        ) {
                            #muttest_api::ControlFlow::Continue(_) => #block,
                            #muttest_api::ControlFlow::Break(_) => {
                                #[allow(unused_imports)]
                                use #muttest_api::mutable::extreme::{NotDefault, YesDefault};
                                (&ret_type).get_default()
                            },
                        }
                    }
                }
            }
        }
    }
}

#[cfg_attr(test, muttest_codegen::mutate_selftest)]
pub fn run(m_id: BakedMutableId) -> ControlFlow<()> {

    m_id.report_coverage(None);

    match m_id.get_active_mutation().as_option() {
        None => ControlFlow::Continue(()),
        Some("default") => ControlFlow::Break(()),
        Some("panic") => panic!("panic from extreme mutation"),
        _ => todo!(),
    }
}

pub fn phantom_unwrap<T>(_: PhantomData<T>) -> T {
    unreachable!()
}

pub trait NotDefault<T> {
    fn is_default(&self) -> bool;
    fn get_default(&self) -> T;
}
pub trait YesDefault<T> {
    fn is_default(&self) -> bool;
    fn get_default(&self) -> T;
}
impl<T> NotDefault<T> for &PhantomData<T> {
    fn is_default(&self) -> bool {
        false
    }
    fn get_default(&self) -> T {
        panic!();
    }
}
impl<T: Default> YesDefault<T> for PhantomData<T> {
    fn is_default(&self) -> bool {
        true
    }
    fn get_default(&self) -> T {
        T::default()
    }
}

#[cfg(test)]
mod tests {
    use std::any::Any;

    use crate::tests::*;

    #[test]
    fn set_true() {
        #[muttest_codegen::mutate_isolated("extreme")]
        fn f(marker: &mut bool) {
            *marker = true;
        }

        let mut b = false;
        call_isolated! {f(&mut b)};
        assert_eq!(b, true);
        let mut b = false;
        call_isolated! {f(&mut b) where 1 => "default"};
        assert_eq!(b, false);
    }

    #[test]
    #[should_panic]
    fn panic_mutation() {
        #[muttest_codegen::mutate_isolated("extreme")]
        fn f() {}

        call_isolated! {f() where 1 => "panic"};
    }

    #[test]

    fn post_increment() {
        #[muttest_codegen::mutate_isolated("extreme")]
        fn f(x: &mut i8) -> i8 {
            let ret = *x;
            *x += 1;
            ret
        }

        let mut x = 1;
        let res = call_isolated! {f(&mut x)};
        assert_eq!(res.res, 1);
        assert_eq!(x, 2);

        let mut x = 1;
        let res = call_isolated! {f(&mut x) where 1 => "default"};
        assert_eq!(res.res, 0);
        assert_eq!(x, 1);
    }

    #[derive(Debug, PartialEq, Eq)]
    struct NoDefault;

    #[test]
    fn no_default() {
        #[muttest_codegen::mutate_isolated("extreme")]
        fn f() -> NoDefault {
            NoDefault
        }
        let res = call_isolated! {f()};
        assert_eq!(
            &res.report
                .for_mutable(1)
                .analysis
                .mutations
                .as_ref()
                .unwrap()
                .to_vec_deref(),
            &["panic"],
        )
    }

    #[test]
    fn return_no_default() {
        #[muttest_codegen::mutate_isolated("extreme")]
        fn f() -> impl std::fmt::Debug {
            return NoDefault;
        }
        let res = call_isolated! {f()};
        assert_eq!(
            &res.report
                .for_mutable(1)
                .analysis
                .mutations
                .as_ref()
                .unwrap()
                .to_vec_deref(),
            &["panic"],
        )
    }

    #[test]
    fn no_default_impl_debug() {
        #[muttest_codegen::mutate_isolated("extreme")]
        fn f() -> impl std::fmt::Debug {
            NoDefault
        }

        let res = call_isolated! {f()};
        assert_eq!(
            res.report
                .for_mutable(1)
                .analysis
                .mutations
                .as_ref()
                .unwrap()
                .to_vec_deref(),
            &["panic"],
        );
        let res: Box<dyn Any> = Box::new(res.res);
        assert_eq!(NoDefault, *res.downcast::<NoDefault>().unwrap());
    }

    #[test]
    fn impl_default() {
        #[muttest_codegen::mutate_isolated("extreme")]
        fn f() -> impl Default + Sized {
            4usize
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
            &["default", "panic"]
        );
        let res: Box<dyn Any> = Box::new(res.res);
        assert_eq!(*res.downcast::<usize>().unwrap(), 4);
        let res = call_isolated! {f() where 1 => "default"};

        let res: Box<dyn Any> = Box::new(res.res);
        assert_eq!(*res.downcast::<usize>().unwrap(), 0);
    }
}
