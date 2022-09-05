use std::cmp::Ordering;

use proc_macro2::{Span, TokenStream};
use quote::{quote_spanned, ToTokens};

use crate::{
    transformer::{Mutable, MuttestTransformer, TransformSnippets},
    MutableId,
};

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

    fn transform(self, transformer: &mut MuttestTransformer) -> TokenStream {
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
                let (_left, _right) = (#left, #right);
                // for type-inference, keep the original expression in the first branch
                if false {_left #op _right} else {
                    #muttest_api::mutable::binop_cmp::run(&#m_id, #op_str, &_left, &_right)
                }
            })
        }
    }
}

pub fn run<T: PartialOrd<T1>, T1>(
    m_id: &MutableId<'static>,
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
        assert_eq!(
            &res.data.coverage[&mutable_id(1)].iter().collect::<Vec<_>>(),
            &["LT"]
        );
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
        assert_eq!(
            &res.data.coverage[&mutable_id(1)].iter().collect::<Vec<_>>(),
            &["EQ", "LT"]
        );
        let res = call_isolated! {f() where 1 => "<="};
        assert_eq!(3, res.res);
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
        assert_ne!(res.data.mutables[&mutable_id(1)].details, None);
        assert_eq!(res.data.coverage.get(&mutable_id(1)), None);
    }
}
