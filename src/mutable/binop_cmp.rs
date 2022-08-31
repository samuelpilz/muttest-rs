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
            core_crate,
            loc,
        } = transformer.new_mutable::<Self>(&op_str, span);

        quote_spanned! {span=>
            ({
                let (left, right) = (#left, #right);
                // for type-inference, keep the original expression in the first branch
                if false {left #op right} else {
                    #core_crate::mutable::binop_cmp::mutable_cmp(&#m_id, #op_str, &left, &right, #loc)
                }
            },).0
        }
    }
}

pub fn mutable_cmp<T: PartialOrd<T1>, T1>(
    m_id: &MutableId<'static>,
    op_str: &str,
    left: &T,
    right: &T1,
    loc: MutableLocation,
) -> bool {
    m_id.report_at(loc);

    let ord = left.partial_cmp(right);
    m_id.report_weak(|s| update_weak_str(ord, s));
    if let Some(ord) = ord {
        match m_id.get_active_mutation().as_deref().unwrap_or(op_str) {
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

fn update_weak_str(ord: Option<Ordering>, s: &str) -> Option<String> {
    if s.is_empty() {
        return Some(ord_to_str(ord).to_owned());
    }
    // parse existing weak-reports
    let mut opts = parse_coverage(s);

    if opts.contains(&ord) {
        return None;
    }
    opts.push(ord);
    // TODO: use `iter::intersperse` when stable
    Some(
        opts.into_iter()
            .map(ord_to_str)
            .collect::<Vec<_>>()
            .join(":"),
    )
}
pub fn parse_coverage(s: &str) -> Vec<Option<Ordering>> {
    s.split(":").map(|s| ord_from_str(s)).collect::<Vec<_>>()
}
fn ord_to_str(ord: Option<Ordering>) -> &'static str {
    match ord {
        None => "",
        Some(Ordering::Less) => "LT",
        Some(Ordering::Equal) => "EQ",
        Some(Ordering::Greater) => "GT",
    }
}
// TODO: report parse errors?
fn ord_from_str(s: &str) -> Option<Ordering> {
    match s {
        "LT" => Some(Ordering::Less),
        "EQ" => Some(Ordering::Equal),
        "GT" => Some(Ordering::Greater),
        _ => None,
    }
}

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
        assert_eq!(&res.data.coverage[&mutable_id(1)], "LT")
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
        assert_eq!(&res.data.coverage[&mutable_id(1)], "LT:EQ")
    }
}
