use std::{
    ops::{Deref, DerefMut},
    sync::RwLock,
};

use proc_macro2::{Span, TokenStream};
use quote::{quote_spanned, ToTokens};
use syn::{Expr, ExprLit, Lit};

use crate::{
    transformer::{MuttestTransformer, TransformSnippets},
    BakedLocation, BakedMutableId,
};

use super::FilterMutableCode;

pub struct Mutable<'a> {
    pub value: String,
    pub span: Span,
    pub lit: &'a dyn ToTokens,
}

impl<'a> TryFrom<&'a Expr> for Mutable<'a> {
    type Error = ();
    fn try_from(expr: &'a Expr) -> Result<Self, ()> {
        match expr {
            Expr::Lit(ExprLit {
                lit: Lit::Str(l), ..
            }) => Ok(Self {
                value: l.value(),
                span: l.span(),
                lit: l,
            }),
            _ => Err(()),
        }
    }
}

impl<'a> super::Mutable<'a> for Mutable<'a> {
    const NAME: &'static str = "lit_str";

    fn span(&self) -> Span {
        self.span
    }

    fn transform(self, transformer: &mut MuttestTransformer) -> TokenStream {
        let span = self.span;
        let lit = self.lit;

        let TransformSnippets {
            m_id,
            muttest_api,
            loc,
        } = transformer.new_mutable(&self, &format!("{:?}", self.value));
        quote_spanned! {span=>
            #muttest_api::mutable::lit_str::run(#m_id, #lit, #loc, {
                static MUTABLE: #muttest_api::RwLock<#muttest_api::Option<&str>> =
                    #muttest_api::RwLock::new(#muttest_api::Option::None);
                &MUTABLE
            })
        }
    }

    fn mutations(analysis: &crate::report::MutableAnalysis) -> Vec<String> {
        [r#""""#, r#""ABC""#]
            .into_iter()
            .filter_mutable_code(&analysis.code)
    }
}

#[cfg_attr(feature = "selftest", muttest::mutate)]
pub fn run(
    m_id: BakedMutableId,
    s: &'static str,
    loc: BakedLocation,
    mutation_store: &RwLock<Option<&'static str>>,
) -> &'static str {
    let mutation = m_id.get_active_mutation();
    if mutation.skip {
        return s;
    }

    mutation.report_coverage(None);

    mutation.report_details(loc, "&'static str", "");

    match mutation.as_option() {
        None => s,
        Some("") => "",
        Some(s_mut) => {
            // TODO: unescape the string

            let r_lock = mutation_store.read().unwrap();
            match r_lock.deref() {
                Some(s_lock) if s_lock == &s_mut => return s_lock,
                _ => {}
            }
            // update the content of the lock
            std::mem::drop(r_lock);
            let mut w_lock = mutation_store.write().unwrap();
            match w_lock.deref_mut() {
                // check if someone else has done the update
                Some(s_lock) if s_lock == &s_mut => return s_lock,
                _ => {}
            }
            // yes, this leaks. but only once per mutation.
            let boxed_str: Box<str> = Box::from(s_mut);
            let leaked = Box::leak(boxed_str);
            *w_lock = Some(leaked);
            leaked
        }
    }
}

#[cfg(test)]
mod tests {

    use crate::{mutable::Mutable, tests::*};

    #[test]
    fn empty_str() {
        #[muttest_codegen::mutate_isolated("lit_str")]
        fn f() -> &'static str {
            ""
        }
        let data = data_isolated!(f);
        assert_eq!(data.mutables.len(), 1);

        assert_eq!(call_isolated! {f()}.res, "");
        assert_eq!(call_isolated! {f() where 1 => "x"}.res, "x");
    }

    #[test]
    fn some_str() {
        #[muttest_codegen::mutate_isolated("lit_str")]
        fn f() -> &'static str {
            "mutation testing!"
        }
        let data = data_isolated!(f);
        assert_eq!(data.mutables.len(), 1);

        assert_eq!(call_isolated! {f()}.res, "mutation testing!");
        assert_eq!(call_isolated! {f() where 1 => ""}.res, "");
    }

    #[test]
    fn patterns_not_mutated() {
        #[muttest_codegen::mutate_isolated("lit_str")]
        fn _f(x: String) {
            match &*x {
                "a" => {}
                "xyz" => {}
                _ => {}
            }
        }
        let data = data_isolated!(_f);
        assert_eq!(data.mutables.len(), 0);
    }

    #[test]
    fn no_empty_string_mutation_for_empty_string() {
        #[muttest_codegen::mutate_isolated("lit_str")]
        fn _f() -> &'static str {
            ""
        }
        let report = data_isolated!(_f);
        assert_eq!(report.mutables.len(), 1);

        let mutations =
            super::Mutable::mutations(&report.mutables.values().next().unwrap().analysis);

        for m in mutations {
            assert_ne!(m, r#""""#);
        }
    }
}
