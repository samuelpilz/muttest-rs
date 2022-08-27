use proc_macro2::{Span, TokenStream};
use quote::{quote_spanned, ToTokens};

use crate::{
    transformer::{display_span, Mutable, MuttestTransformer},
    *,
};

pub struct MutableBinopCmp<'a> {
    pub left: &'a dyn ToTokens,
    pub right: &'a dyn ToTokens,
    pub op: &'a dyn ToTokens,
    pub span: Span,
}

impl<'a> Mutable<'a> for MutableBinopCmp<'a> {
    const NAME: &'static str = "binop_cmp";

    fn transform(self, transformer: &mut MuttestTransformer) -> TokenStream {
        let span = self.span;
        let op = self.op.to_token_stream();
        let op_str = op.to_string();
        let m_id = transformer.register_new_mutable(Self::NAME, &op_str, &display_span(span));
        let m_id = transformer.mutable_id_expr(&m_id, span);
        let core_crate = transformer.core_crate_path(span);
        let (left, right) = (self.left, self.right);
        quote_spanned! {span=>
            ({
                #core_crate::report_location(&#m_id, file!(), line!(), column!());
                let (left, right) = (#left, #right);
                // for type-inference, keep the original expression in the first branch
                if false {left #op right} else {
                    #core_crate::mutable::binop_cmp::mutable_cmp(&#m_id, #op_str, &left, &right)
                }
            },).0
        }
    }
}

pub fn mutable_cmp<T: PartialOrd<T1>, T1>(
    m_id: &MutableId,
    op_str: &str,
    left: &T,
    right: &T1,
) -> bool {
    report_coverage(m_id);
    let ord = left.partial_cmp(right);
    // TODO: record behavior for weak mutation testing
    // save_msg(&format!("CMP {m_id}; {ord:?}"));
    if let Some(ord) = ord {
        match get_active_mutation_for_mutable(m_id)
            .as_deref()
            .unwrap_or(op_str)
        {
            "<" => ord.is_lt(),
            "<=" => ord.is_le(),
            ">=" => ord.is_ge(),
            ">" => ord.is_gt(),
            _ => todo!(),
        }
    } else {
        false
    }
}

#[cfg(test)]
mod tests {
    #[muttest_codegen::mutate_isolated("binop_cmp")]
    fn lt_ints() -> bool {
        1 < 2
    }

    #[test]
    fn lt_ints_mutables() {
        assert_eq!(lt_ints::NUM_MUTABLES, 1);
        assert_eq!(lt_ints::MUTABLES_CSV.lines().count(), 2);
    }

    #[test]
    fn lt_ints_unchanged() {
        assert_eq!(true, crate::tests::without_mutation(lt_ints));
    }

    #[test]
    fn lt_ints_gt() {
        assert_eq!(false, crate::tests::with_mutation(1, ">", lt_ints));
    }

    #[test]
    fn lt_ints_ge() {
        assert_eq!(false, crate::tests::with_mutation(1, ">=", lt_ints));
    }
}
