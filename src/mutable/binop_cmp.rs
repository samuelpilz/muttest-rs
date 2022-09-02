use std::cmp::Ordering;

use proc_macro2::{Span, TokenStream};
use quote::{quote_spanned, ToTokens};

use crate::{
    transformer::{Mutable, MuttestTransformer, TransformSnippets},
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
        let (left, right) = (self.left, self.right);

        let TransformSnippets {
            m_id,
            muttest_api,
            loc,
        } = transformer.new_mutable::<Self>(&op_str, span);

        quote_spanned! {span=>
            ({
                let (left, right) = (#left, #right);
                // for type-inference, keep the original expression in the first branch
                if false {left #op right} else {
                    #muttest_api::mutable::binop_cmp::run(&#m_id, #op_str, &left, &right, #loc)
                }
            },).0
        }
    }
}

pub fn run<T: PartialOrd<T1>, T1>(
    m_id: &MutableId<'static>,
    op_str: &str,
    left: &T,
    right: &T1,
    loc: MutableLocation,
) -> bool {
    m_id.report_at(loc);

    let ord = left.partial_cmp(right);
    m_id.report_weak(ord_to_str(ord));

    match (ord, m_id.get_active_mutation().as_deref().unwrap_or(op_str)) {
        (None, _) => false,
        (Some(ord), "<") => ord.is_lt(),
        (Some(ord), "<=") => ord.is_le(),
        (Some(ord), ">=") => ord.is_ge(),
        (Some(ord), ">") => ord.is_gt(),
        _ => todo!(),
    }
}

fn ord_to_str(ord: Option<Ordering>) -> &'static str {
    match ord {
        None => "",
        Some(Ordering::Less) => "LT",
        Some(Ordering::Equal) => "EQ",
        Some(Ordering::Greater) => "GT",
    }
}

// TODO: move weak-check to here

#[cfg(test)]
mod tests {
    use crate::tests::mutable_id;

    #[muttest_codegen::mutate_isolated("binop_cmp")]
    fn lt_ints() -> bool {
        1 < 2
    }

    // TODO: parse csv into collected data instead
    #[test]
    fn lt_ints_mutables() {
        assert_eq!(lt_ints::NUM_MUTABLES, 1);
        assert_eq!(lt_ints::MUTABLES_CSV.lines().count(), 2);
    }

    #[test]
    fn lt_ints_unchanged_weak_lt() {
        let res = crate::tests::without_mutation(lt_ints);
        assert_eq!(true, res.res);
        assert_eq!(
            &res.data.coverage[&mutable_id(1)].iter().collect::<Vec<_>>(),
            &["LT"]
        )
    }

    #[test]
    fn lt_ints_gt() {
        assert_eq!(false, crate::tests::with_mutation(1, ">", lt_ints).res);
    }

    #[test]
    fn lt_ints_ge() {
        assert_eq!(false, crate::tests::with_mutation(1, ">=", lt_ints).res);
    }

    #[muttest_codegen::mutate_isolated("binop_cmp")]
    fn count_up() -> u8 {
        let mut x = 1;
        while x < 2 {
            x += 1;
        }
        x
    }

    #[test]
    fn count_up_unchanged_weak_lt_and_eq() {
        let res = crate::tests::without_mutation(count_up);
        assert_eq!(2, res.res);
        assert_eq!(
            &res.data.coverage[&mutable_id(1)].iter().collect::<Vec<_>>(),
            &["EQ", "LT"]
        )
    }
}
