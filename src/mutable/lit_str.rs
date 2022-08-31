use std::{
    ops::{Deref, DerefMut},
    sync::RwLock,
};

use proc_macro2::{Span, TokenStream};
use quote::{quote_spanned, ToTokens};

use crate::{
    transformer::{Mutable, MuttestTransformer, TransformSnippets},
    *,
};

pub struct MutableLitStr<'a> {
    pub value: String,
    pub span: Span,
    pub lit: &'a dyn ToTokens,
}

impl<'a> Mutable<'a> for MutableLitStr<'a> {
    const NAME: &'static str = "lit_str";

    fn transform(self, transformer: &mut MuttestTransformer) -> TokenStream {
        let span = self.span;
        let lit = self.lit;

        let TransformSnippets {
            m_id,
            core_crate,
            loc,
        } = transformer.new_mutable::<Self>(&self.value, span);
        quote_spanned! {span=>
            #core_crate::mutable::lit_str::mutable_str(&#m_id, #lit, #loc, {
                static MUTABLE: ::std::sync::RwLock<::std::option::Option<&str>> =
                    ::std::sync::RwLock::new(::std::option::Option::None);
                &MUTABLE
            })
        }
    }
}

pub fn mutable_str(
    m_id: &MutableId<'static>,
    s: &'static str,
    loc: MutableLocation,
    mutation: &RwLock<Option<&'static str>>,
) -> &'static str {
    m_id.report_at(loc);

    match m_id.get_active_mutation().as_deref() {
        None => s,
        Some(s_mut) => {
            let r_lock = mutation.read().unwrap();
            match r_lock.deref() {
                Some(s_lock) if s_lock == &s_mut => return s_lock,
                _ => {}
            }
            // update the content of the lock
            std::mem::drop(r_lock);
            let mut w_lock = mutation.write().unwrap();
            match w_lock.deref_mut() {
                // check if someone else has done the update
                Some(s_lock) if s_lock == &s_mut => return s_lock,
                _ => {}
            }
            // yes, this leaks. but only once per mutation.
            // NB: this does not leak for the empty string
            let boxed_str: Box<str> = Box::from(s_mut);
            let leaked = Box::leak(boxed_str);
            println!("mutated: {leaked:?}");
            *w_lock = Some(leaked);
            leaked
        }
    }
}

#[cfg(test)]
mod tests {

    #[muttest_codegen::mutate_isolated("lit_str")]
    fn empty_str() -> &'static str {
        ""
    }
    #[test]
    fn empty_str_mutables() {
        assert_eq!(empty_str::NUM_MUTABLES, 1);
        assert_eq!(empty_str::MUTABLES_CSV.lines().count(), 2);
    }

    #[test]
    fn empty_str_unchanged() {
        assert_eq!(crate::tests::without_mutation(empty_str).res, "");
    }

    #[test]
    fn empty_str_one() {
        assert_eq!(crate::tests::with_mutation(1, "1", empty_str).res, "1");
    }

    #[muttest_codegen::mutate_isolated("lit_str")]
    fn some_str() -> &'static str {
        "mutation testing!"
    }

    #[test]
    fn some_str_mutables() {
        assert_eq!(some_str::NUM_MUTABLES, 1);
        assert_eq!(some_str::MUTABLES_CSV.lines().count(), 2);
    }

    #[test]
    fn some_str_unchanged() {
        assert_eq!(
            crate::tests::without_mutation(some_str).res,
            "mutation testing!"
        );
    }

    #[test]
    fn some_str_empty() {
        assert_eq!(crate::tests::with_mutation(1, "", some_str).res, "");
    }
}
