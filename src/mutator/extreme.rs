use std::ops::ControlFlow;

use quote::quote_spanned;
use syn::{
    visit::Visit, Attribute, Block, ImplItemFn, ItemFn, ReturnType, Signature, Token, Type,
    TypeImplTrait, Visibility,
};

use crate::{
    internal_error,
    report::{MutableAnalysis, MutableId},
    transformer::{Mutated, MuttestTransformer, TransformSnippets},
    Error,
};

use super::MutatorFns;

pub struct Mutator;
// TODO: mutate (some) blocks instead of complete `fn`s?

#[cfg_attr(feature = "selftest", muttest::mutate)]
impl MutatorFns for Mutator {
    fn valid_mutations(&self, analysis: &MutableAnalysis) -> Vec<String> {
        if let Some(m) = &analysis.mutations {
            m.clone()
        } else if analysis.ty.as_deref() == Some("()") {
            vec!["default".to_owned(), "panic".to_owned()]
        } else {
            vec!["panic".to_owned()]
        }
    }

    #[cfg_attr(muttest, ignore)]
    fn transform_item_fn(
        &self,
        transformer: &mut MuttestTransformer,
        i: &ItemFn,
    ) -> ControlFlow<Mutated> {
        if matches!(&i.sig.output, ReturnType::Type(_, t) if is_existential_type(t)) {
            return ControlFlow::Continue(());
        }
        ControlFlow::Break(
            Mutable {
                attrs: &i.attrs,
                vis: &i.vis,
                sig: &i.sig,
                block: &i.block,
                defaultness: None,
            }
            .transform(transformer),
        )
    }

    #[cfg_attr(muttest, ignore)]
    fn transform_impl_item_fn(
        &self,
        transformer: &mut MuttestTransformer,
        i: &ImplItemFn,
    ) -> ControlFlow<Mutated> {
        if matches!(&i.sig.output, ReturnType::Type(_, t) if is_existential_type(t)) {
            return ControlFlow::Continue(());
        }
        ControlFlow::Break(
            Mutable {
                attrs: &i.attrs,
                vis: &i.vis,
                sig: &i.sig,
                block: &i.block,
                defaultness: i.defaultness,
            }
            .transform(transformer),
        )
    }
}

pub struct Mutable<'a> {
    attrs: &'a [Attribute],
    vis: &'a Visibility,
    sig: &'a Signature,
    block: &'a Block,
    defaultness: Option<Token![default]>,
}

impl Mutable<'_> {
    fn transform(self, transformer: &mut MuttestTransformer) -> Mutated {
        let Self {
            attrs,
            vis,
            sig,
            block,
            defaultness,
        } = self;
        let span = sig.ident.span();
        let TransformSnippets { m_id, muttest_api } = transformer.new_mutable(span);

        let transformed = quote_spanned! {span=>
            #(#attrs)* #vis #defaultness #sig {
                let ret_type = #muttest_api::PhantomData;
                #[allow(unreachable_code)] // warning appears for output of type `!`
                if false {
                    return #muttest_api::phantom_unwrap(ret_type);
                }

                // report static analysis
                (#m_id).write_types(
                    "",
                    #muttest_api::PossibleMutations(
                        &[
                            ("default",
                            {
                                use #muttest_api::mutator::extreme::is_default::IsDefault;
                                (&&ret_type).is_default()
                            }),
                            ("panic", true),
                        ]
                    )
                );

                match #muttest_api::mutator::extreme::run(#m_id) {
                    #muttest_api::ControlFlow::Continue(_) => #block,
                    #muttest_api::ControlFlow::Break(_) => {
                        use #muttest_api::mutator::extreme::is_default::IsDefault;
                        (&&ret_type).get_default()
                    },
                }
            }
        };
        Mutated {
            transformed,
            code: format!("fn {}", sig.ident),
            span,
            type_info: matches!(sig.output, ReturnType::Default).then(|| "()".to_owned()),
        }
    }
}

#[cfg_attr(feature = "selftest", muttest::mutate)]
pub fn run(m_id: MutableId) -> ControlFlow<()> {
    m_id.write_coverage();

    match m_id.get_action() {
        None => ControlFlow::Continue(()),
        Some("default") => ControlFlow::Break(()),
        Some("panic") => panic!("panic from extreme mutation"),
        // TODO: unit-test this
        Some(m) => internal_error(Error::InvalidMutation(m)),
    }
}

fn is_existential_type(t: &Type) -> bool {
    struct V(bool);
    impl<'a> Visit<'a> for V {
        fn visit_type_impl_trait(&mut self, _: &'a TypeImplTrait) {
            self.0 = true;
        }
    }
    let mut v = V(false);
    v.visit_type(t);
    v.0
}

pub mod is_default {
    use std::marker::PhantomData;

    pub trait IsDefault<T> {
        fn is_default(&self) -> bool;
        fn get_default(&self) -> T;
    }
    impl<T> IsDefault<T> for PhantomData<T> {
        fn is_default(&self) -> bool {
            false
        }
        fn get_default(&self) -> T {
            panic!();
        }
    }
    impl<T: Default> IsDefault<T> for &PhantomData<T> {
        fn is_default(&self) -> bool {
            true
        }
        fn get_default(&self) -> T {
            T::default()
        }
    }
}

#[cfg(test)]
#[cfg_attr(feature = "selftest", muttest::tests)]
mod tests {
    use super::Mutator;
    use crate::{mutator::MutatorFns, tests::*};

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
        call_isolated! {f(&mut b) where 1: "default"};
        assert_eq!(b, false);
    }

    #[test]
    #[should_panic]
    fn panic_mutation() {
        #[muttest_codegen::mutate_isolated("extreme")]
        fn f() {}

        call_isolated! {f() where 1: "panic"};
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
        let res = call_isolated! {f(&mut x) where 1: "default"};
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
    fn unit_return_mutated_without_coverage() {
        #[muttest_codegen::mutate_isolated("extreme")]
        fn _f() {}

        let data = data_isolated!(_f);
        assert_eq!(data.mutables.len(), 1);
        assert_eq!(
            Mutator
                .valid_mutations(&data.for_mutable(1).analysis)
                .to_vec_deref(),
            vec!["default", "panic"]
        )
    }

    #[test]
    fn int_function_mutated_to_panic_without_coverage() {
        #[muttest_codegen::mutate_isolated("extreme")]
        fn _f() -> i32 {
            1
        }

        let data = data_isolated!(_f);
        assert_eq!(data.mutables.len(), 1);
        assert_eq!(
            Mutator
                .valid_mutations(&data.for_mutable(1).analysis)
                .to_vec_deref(),
            vec!["panic"]
        )
    }

    #[test]
    fn impl_default_not_mutated() {
        #[muttest_codegen::mutate_isolated("extreme")]
        fn _f() -> impl Default + Sized {
            4usize
        }

        let data = data_isolated!(_f);
        assert_eq!(data.mutables.len(), 0);
    }

    #[test]
    fn impl_iter_not_mutated() {
        #[muttest_codegen::mutate_isolated("extreme")]
        fn _f() -> impl Iterator<Item = String> {
            [1].iter().map(|&s| s.to_string())
        }

        let data = data_isolated!(_f);
        assert_eq!(data.mutables.len(), 0);
    }

    #[test]
    fn async_function() {
        use futures::executor::block_on;

        #[muttest_codegen::mutate_isolated("extreme")]
        pub async fn f() -> u32 {
            async { 1 }.await
        }

        // default mutation is possible
        let res = call_isolated! {f: block_on(f())};
        assert_eq!(res.res, 1);
        assert_eq!(
            res.report
                .for_mutable(1)
                .analysis
                .mutations
                .as_ref()
                .unwrap()
                .to_vec_deref(),
            &["default", "panic"],
        );

        let res = call_isolated! {f: block_on(f()); where 1: "default"};
        assert_eq!(res.res, 0);
    }

    #[test]
    fn impl_item_fn_mutated() {
        #[muttest_codegen::mutate_isolated("extreme")]
        mod x {
            pub struct X;
            impl X {
                pub fn x() -> u32 {
                    1
                }
            }
        }

        let data = data_isolated!(x);
        assert_eq!(data.mutables.len(), 1);

        let res = call_isolated! {x: x::X::x()};
        assert_eq!(res.res, 1);
        let res = call_isolated! {x: x::X::x(); where 1: "default"};
        assert_eq!(res.res, 0);
    }

    #[test]
    fn correct_backwards_type_inference() {
        #[muttest_codegen::mutate_isolated("extreme")]
        pub fn f1() -> Option<&'static [u8]> {
            Some(&[])
        }
        #[muttest_codegen::mutate_isolated("extreme")]
        pub fn f2() -> usize {
            1 << 1u32
        }

        let res = call_isolated! {f1()};
        assert_eq!(res.res, Some(&*vec![]));
        assert_eq!(
            res.report
                .for_mutable(1)
                .analysis
                .mutations
                .as_ref()
                .unwrap()
                .to_vec_deref(),
            &["default", "panic"],
        );
        let res = call_isolated! {f1() where 1: "default"};
        assert_eq!(res.res, None);

        let res = call_isolated! {f2()};
        assert_eq!(res.res, 2);
        assert_eq!(
            res.report
                .for_mutable(1)
                .analysis
                .mutations
                .as_ref()
                .unwrap()
                .to_vec_deref(),
            &["default", "panic"],
        );
        let res = call_isolated! {f2() where 1: "default"};
        assert_eq!(res.res, 0);
    }
}
