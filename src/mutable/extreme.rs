use std::ops::ControlFlow;

use proc_macro2::{Span, TokenStream};
use quote::{quote_spanned, ToTokens};

use crate::{
    transformer::{Mutable, MuttestTransformer, TransformSnippets},
    *,
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

    fn transform(self, transformer: &mut MuttestTransformer) -> TokenStream {
        let span = self.span;
        let TransformSnippets {
            m_id,
            core_crate,
            loc,
        } = transformer.new_mutable::<Self>("", span);
        // TODO: add reasonable code for that
        // TODO: or should code be optional?

        let MutableExtreme {
            vis, sig, block, ..
        } = self;
        quote_spanned! {span=>
            #vis #sig {
                let ret_type = ::std::marker::PhantomData;
                match #core_crate::mutable::extreme::run(&#m_id, #loc) {
                    ::std::ops::ControlFlow::Continue(_) => {
                        // help the type-checker
                        match 0 {
                            // type-check the original code first
                            1 => #block,
                            2 => #core_crate::mutable::extreme::phantom_unwrap(ret_type),
                            3 => return #core_crate::mutable::extreme::phantom_unwrap(ret_type),
                            // report the type first, then execute the block
                            _ => {
                                (#m_id).report_possible_mutations(
                                    &[("default",
                                        {
                                            #[allow(unused_imports)]
                                            use #core_crate::mutable::extreme::{NotDefault, YesDefault};
                                            (&ret_type).is_default()
                                        }
                                    )]
                                );
                                #block
                            }
                        }
                    }
                    ::std::ops::ControlFlow::Break(_) => {
                        #[allow(unused_imports)]
                        use #core_crate::mutable::extreme::{NotDefault, YesDefault};
                        (&ret_type).get_default()
                    },
                }
            }
        }
    }
}

#[derive(Debug)]
struct NoDefault;

pub fn phantom_unwrap<T>(_: PhantomData<T>) -> T {
    panic!()
}

// TODO: private names for these functions?
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

pub fn run(m_id: &MutableId<'static>, loc: MutableLocation) -> ControlFlow<()> {
    m_id.report_at(loc);

    match m_id.get_active_mutation().as_deref().unwrap_or_default() {
        "" => ControlFlow::Continue(()),
        "default" => ControlFlow::Break(()),
        _ => todo!(),
    }
}

#[cfg(test)]
mod tests {
    use std::any::Any;

    use crate::tests::mutable_id;

    #[muttest_codegen::mutate_isolated("extreme")]
    fn set_true(marker: &mut bool) {
        *marker = true;
    }

    #[test]
    fn set_true_unchanged() {
        let mut b = false;
        crate::tests::without_mutation(|| set_true(&mut b));
        assert_eq!(b, true);
    }

    #[test]
    fn set_true_default() {
        let mut b = false;
        crate::tests::with_mutation(1, "default", || set_true(&mut b));
        assert_eq!(b, false);
    }

    #[muttest_codegen::mutate_isolated("extreme")]
    fn post_increment(x: &mut i8) -> i8 {
        let ret = *x;
        *x += 1;
        ret
    }

    #[test]
    fn post_increment_unchanged() {
        let mut x = 1;
        let res = crate::tests::without_mutation(|| post_increment(&mut x));
        assert_eq!(res.res, 1);
        assert_eq!(x, 2);
    }

    #[test]
    fn post_increment_mutate_default() {
        let mut x = 1;
        let res = crate::tests::with_mutation(1, "default", || post_increment(&mut x));
        assert_eq!(res.res, 0);
        assert_eq!(x, 1);
    }

    #[derive(Debug, PartialEq, Eq)]
    struct NoDefault;

    #[muttest_codegen::mutate_isolated("extreme")]
    fn no_default() -> NoDefault {
        NoDefault
    }
    #[test]
    fn no_default_no_mutation() {
        let res = crate::tests::without_mutation(no_default);
        assert_eq!(
            res.data
                .mutables
                .get(&mutable_id(1))
                .and_then(|x| x.possible_mutations.as_ref()),
            Some(&vec![])
        )
    }

    #[muttest_codegen::mutate_isolated("extreme")]
    fn return_no_default_impl_debug() -> impl std::fmt::Debug {
        return NoDefault;
    }
    #[test]
    fn return_no_default_no_mutation() {
        let res = crate::tests::without_mutation(return_no_default_impl_debug);
        assert_eq!(
            res.data
                .mutables
                .get(&mutable_id(1))
                .and_then(|x| x.possible_mutations.as_ref()),
            Some(&vec![])
        )
    }

    #[muttest_codegen::mutate_isolated("extreme")]
    fn no_default_impl_debug() -> impl std::fmt::Debug {
        NoDefault
    }
    #[test]
    fn no_default_impl_debug_unchanged_no_mutation() {
        let res = crate::tests::without_mutation(no_default_impl_debug);
        assert_eq!(
            res.data
                .mutables
                .get(&mutable_id(1))
                .and_then(|x| x.possible_mutations.as_ref()),
            Some(&vec![])
        );
        let res: Box<dyn Any> = Box::new(res.res);
        assert_eq!(NoDefault, *res.downcast::<NoDefault>().unwrap());
    }

    #[muttest_codegen::mutate_isolated("extreme")]
    fn impl_default() -> impl Default + Sized {
        4usize
    }
    #[test]
    fn impl_default_unchanged_one_mutation() {
        let res = crate::tests::without_mutation(impl_default);
        assert_eq!(
            res.data
                .mutables
                .get(&mutable_id(1))
                .and_then(|x| x.possible_mutations.as_ref()),
            Some(&vec!["default".to_owned()])
        );
        let res: Box<dyn Any> = Box::new(res.res);
        assert_eq!(*res.downcast::<usize>().unwrap(), 4);
    }
    #[test]
    fn impl_default_mutate_default() {
        let res = crate::tests::with_mutation(1, "default", impl_default);
        let res: Box<dyn Any> = Box::new(res.res);
        assert_eq!(*res.downcast::<usize>().unwrap(), 0);
    }
}
