use std::{marker::PhantomData, ops::ControlFlow};

use proc_macro2::{Span, TokenStream};
use quote::{quote_spanned, ToTokens};

use crate::{
    transformer::{Mutable, MuttestTransformer, TransformSnippets},
    MutableId,
};

// TODO: mutate (some) blocks instead of `ItemFn`s
pub struct MutableExtreme<'a> {
    pub vis: &'a dyn ToTokens,
    pub sig: &'a dyn ToTokens,
    pub block: &'a dyn ToTokens,
    pub span: Span,
}

impl<'a> Mutable<'a> for MutableExtreme<'a> {
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
        // TODO: or should code be optional?

        let MutableExtreme {
            vis, sig, block, ..
        } = self;
        quote_spanned! {span=>
            #vis #sig {
                let ret_type = #muttest_api::PhantomData;

                // dead branches help type inference
                match 0 {
                    // type-check the original code first
                    1 => #block,
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
                            #muttest_api::mutation_string_opt("default",
                                {
                                    #[allow(unused_imports)]
                                    use #muttest_api::mutable::extreme::{NotDefault, YesDefault};
                                    (&ret_type).is_default()
                                }
                            )
                        );

                        match #muttest_api::mutable::extreme::run(&#m_id,
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

pub fn run(m_id: &MutableId<'static>) -> ControlFlow<()> {
    match m_id.get_active_mutation().as_deref().unwrap_or_default() {
        "" => ControlFlow::Continue(()),
        "default" => ControlFlow::Break(()),
        _ => todo!(),
    }
}

pub fn phantom_unwrap<T>(_: PhantomData<T>) -> T {
    todo!()
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
            &res.data.mutables[&mutable_id(1)]
                .details
                .as_ref()
                .unwrap()
                .possible_mutations,
            &<Vec<String>>::new(),
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
            &res.data.mutables[&mutable_id(1)]
                .details
                .as_ref()
                .unwrap()
                .possible_mutations,
            &<Vec<String>>::new(),
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
            &res.data.mutables[&mutable_id(1)]
                .details
                .as_ref()
                .unwrap()
                .possible_mutations,
            &<Vec<String>>::new(),
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
            &res.data.mutables[&mutable_id(1)]
                .details
                .as_ref()
                .unwrap()
                .possible_mutations,
            &["default"]
        );
        let res: Box<dyn Any> = Box::new(res.res);
        assert_eq!(*res.downcast::<usize>().unwrap(), 4);
        let res = call_isolated! {f() where 1 => "default"};

        let res: Box<dyn Any> = Box::new(res.res);
        assert_eq!(*res.downcast::<usize>().unwrap(), 0);
    }
}
