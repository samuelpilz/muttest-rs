use std::{cmp::Ordering, collections::BTreeSet, io::Write};

use proc_macro2::{Span, TokenStream};
use quote::{quote_spanned, ToTokens};

use crate::{
    transformer::{MuttestTransformer, TransformSnippets},
    BakedMutableId,
};

use super::Mutable;

pub struct MutableBinopCmp<'a> {
    pub left: &'a dyn ToTokens,
    pub right: &'a dyn ToTokens,
    pub op: &'a dyn ToTokens,
    pub span: Span,
}

impl<'a> Mutable<'a> for MutableBinopCmp<'a> {
    const NAME: &'static str = "binop_cmp";

    fn span(&self) -> Span {
        self.span
    }

    fn transform<W: Write>(self, transformer: &mut MuttestTransformer<W>) -> TokenStream {
        let span = self.span;
        let op = self.op.to_token_stream();
        let op_str = op.to_string();
        let (left, right) = (self.left, self.right);

        let TransformSnippets {
            m_id,
            muttest_api,
            loc,
        } = transformer.new_mutable(&self, &op_str);

        quote_spanned! {span=>
            #muttest_api::id({
                (#m_id).report_details(#loc, "", "<:<=:>=:>");
                let (_left, _right) = (&#left, &#right);
                // for type-inference, keep the original expression in the first branch
                if false {_left #op _right} else {
                    #muttest_api::mutable::binop_cmp::run(#m_id, #op_str, &_left, &_right)
                }
            })
        }
    }
}

#[cfg_attr(test, muttest_codegen::mutate_selftest)]
pub fn run<T: PartialOrd<T1>, T1>(
    m_id: BakedMutableId,
    op_str: &str,
    left: &T,
    right: &T1,
) -> bool {
    let ord = left.partial_cmp(right);
    m_id.report_weak(ord_to_str(ord));

    match (ord, m_id.get_active_mutation().as_deref().unwrap_or(op_str)) {
        (None, _) => false,
        (Some(ord), "<") => ord.is_lt(),
        (Some(ord), "<=") => ord.is_le(),
        (Some(ord), ">=") => ord.is_ge(),
        (Some(ord), ">") => ord.is_gt(),
        _ => todo!(), // TODO: some lock is held here
    }
}

#[cfg_attr(test, muttest_codegen::mutate_selftest)]
pub fn identical_behavior(code: &str, mutation: &str, coverage: &BTreeSet<String>) -> bool {
    // TODO: better structure for this
    (code == "<" && mutation == "<=" && !coverage.contains("EQ"))
        || (code == "<=" && mutation == "<" && !coverage.contains("EQ"))
        || (code == ">" && mutation == ">=" && !coverage.contains("EQ"))
        || (code == ">=" && mutation == ">" && !coverage.contains("EQ"))
        || (code == "<=" && mutation == ">=" && *coverage.iter().collect::<Vec<_>>() == ["EQ"])
        || (code == ">=" && mutation == "<=" && *coverage.iter().collect::<Vec<_>>() == ["EQ"])
        || (code == "<" && mutation == ">" && *coverage.iter().collect::<Vec<_>>() == ["EQ"])
        || (code == ">" && mutation == "<" && *coverage.iter().collect::<Vec<_>>() == ["EQ"])
}
// TODO: test this

#[cfg_attr(test, muttest_codegen::mutate_selftest)]
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
    use crate::tests::*;

    #[test]
    fn lt_ints() {
        #[muttest_codegen::mutate_isolated("binop_cmp")]
        fn f() -> bool {
            1 < 2
        }

        let data = data_isolated!(f);
        assert_eq!(data.mutables.len(), 1);

        let res = call_isolated! {f()};
        assert_eq!(true, res.res);
        assert_eq!(&res.data.coverage[&1].to_vec_ref(), &["LT"]);
        assert_eq!(false, call_isolated! {f() where 1 => ">"}.res);
        assert_eq!(false, call_isolated! {f() where 1 => ">="}.res);
    }

    #[test]
    fn count_up() {
        #[muttest_codegen::mutate_isolated("binop_cmp")]
        fn f() -> u8 {
            let mut x = 1;
            while x < 2 {
                x += 1;
            }
            x
        }

        let res = call_isolated! {f()};
        assert_eq!(2, res.res);
        assert_eq!(&res.data.coverage[&1].to_vec_ref(), &["EQ", "LT"]);
        let res = call_isolated! {f() where 1 => "<="};
        assert_eq!(3, res.res);
    }

    #[test]
    fn compare_unsized() {
        #[muttest_codegen::mutate_isolated("binop_cmp")]
        fn f(s: String) -> bool {
            *s < *"2";
            // second call to ensure that s is not moved in call before
            *s < *"2"
        }

        assert_eq!(call_isolated! {f(String::new())}.res, true);
        assert_eq!(call_isolated! {f(String::new()) where 2 => ">"}.res, false);
    }

    #[test]
    fn compare_tmp_vars() {
        #[muttest_codegen::mutate_isolated("binop_cmp")]
        fn f() -> bool {
            vec!["1", "2", "3"].into_iter().collect::<String>() < "123".to_owned()
        }

        assert_eq!(call_isolated! {f()}.res, false);
        assert_eq!(call_isolated! {f() where 1 => ">"}.res, false);
    }

    #[test]
    fn details_reported_before_covered() {
        #[muttest_codegen::mutate_isolated("binop_cmp")]
        #[allow(unreachable_code)]
        fn f() -> bool {
            1 < ({
                return false;
                2
            })
        }

        let res = call_isolated! {f()};
        assert_eq!(false, res.res);
        assert_ne!(res.data.mutables[&1].details, None);
        assert_eq!(res.data.coverage.get(&1), None);
    }
}
