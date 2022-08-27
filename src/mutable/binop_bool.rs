use proc_macro2::{Span, TokenStream};
use quote::{quote_spanned, ToTokens};

use crate::{
    transformer::{display_span, Mutable, MuttestTransformer},
    *,
};

pub struct MutableBinopBool<'a> {
    pub left: &'a dyn ToTokens,
    pub right: &'a dyn ToTokens,
    pub op: &'a dyn ToTokens,
    pub span: Span,
}

impl<'a> Mutable<'a> for MutableBinopBool<'a> {
    fn transform(self, transformer: &mut MuttestTransformer) -> TokenStream {
        let span = self.span;
        let op = self.op.to_token_stream();
        let op_str = op.to_string();
        let m_id = transformer.register_new_mutable("bool", &op_str, &display_span(span));
        let m_id = transformer.mutable_id_expr(&m_id, span);
        let core_crate = transformer.core_crate_path(span);
        let (left, right) = (self.left, self.right);
        quote_spanned! {span=>
            ({
                #core_crate::report_location(&#m_id, file!(), line!(), column!());
                if let Some(b) = #core_crate::mutable::binop_bool::run(&#m_id, #op_str, #left) {
                    b
                } else {
                    #right
                }
            },).0
        }
    }
}

pub fn run(m_id: &MutableId, op_str: &str, left: bool) -> Option<bool> {
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
        assert_eq!(crate::tests::without_mutation(true_and_false), false);
    }

    // TODO: tests
}
