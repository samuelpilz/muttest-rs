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
        } = transformer.new_mutable::<Self>("<TODO>", span);

        let MutableExtreme {
            vis,
            sig,
            block,
            ..
        } = self;
        quote_spanned! {span=>
            #vis #sig {
                match #core_crate::mutable::extreme::run(&#m_id, #loc) {
                    ::std::ops::ControlFlow::Continue(_) => #block
                    ::std::ops::ControlFlow::Break(_) => {},
                }
            }
        }
    }
}

pub fn run(m_id: &MutableId<'static>, loc: MutableLocation) -> ControlFlow<()> {
    m_id.report_at(loc);
    report_coverage(m_id);

    match get_active_mutation_for_mutable(m_id)
        .as_deref()
        .unwrap_or_default()
    {
        "" => ControlFlow::Continue(()),
        "default" => ControlFlow::Break(()),
        _ => todo!(),
    }
}

#[cfg(test)]
mod tests {
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

    // TODO: more tests
}
