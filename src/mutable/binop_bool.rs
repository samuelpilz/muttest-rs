use proc_macro2::{Span, TokenStream};
use quote::{quote_spanned, ToTokens};

use crate::{
    transformer::{Mutable, MuttestTransformer, TransformSnippets},
    *,
};

pub struct MutableBinopBool<'a> {
    pub left: &'a dyn ToTokens,
    pub right: &'a dyn ToTokens,
    pub op: &'a dyn ToTokens,
    pub span: Span,
}

impl<'a> Mutable<'a> for MutableBinopBool<'a> {
    const NAME: &'static str = "binop_bool";

    fn transform(self, transformer: &mut MuttestTransformer) -> TokenStream {
        let span = self.span;
        let op = self.op.to_token_stream();
        let op_str = op.to_string();
        let (left, right) = (self.left, self.right);

        let TransformSnippets {
            m_id,
            core_crate,
            loc,
        } = transformer.new_mutable::<Self>(&op_str, span);

        quote_spanned! {span=>
            if let Some(b) = #core_crate::mutable::binop_bool::run(&#m_id, #op_str, #left, #loc) {
                b
            } else {
                #right
            }
        }
    }
}

pub fn run(
    m_id: &MutableId<'static>,
    op_str: &str,
    left: bool,
    loc: MutableLocation,
) -> Option<bool> {
    m_id.report_at(loc);
    report_coverage(m_id);

    match get_active_mutation_for_mutable(m_id)
        .as_deref()
        .unwrap_or(op_str)
    {
        "&&" if left => None,
        "&&" => Some(false),
        "||" if left => Some(true),
        "||" => None,
        _ => todo!(),
    }
}

#[cfg(test)]
mod tests {

    #[muttest_codegen::mutate_isolated("binop_bool")]
    fn true_and_false() -> bool {
        true && false
    }

    #[test]
    fn true_and_false_unchanged() {
        assert_eq!(crate::tests::without_mutation(true_and_false).res, false);
    }

    // TODO: tests
}
