use std::ops::ControlFlow;

use proc_macro2::{Span, TokenStream};
use quote::{quote_spanned, ToTokens};

use crate::{
    transformer::{Mutable, MuttestTransformer, TransformSnippets},
    MutableId,
};

pub struct MutableBinopBool<'a> {
    pub left: &'a dyn ToTokens,
    pub right: &'a dyn ToTokens,
    pub op: &'a dyn ToTokens,
    pub span: Span,
}

impl<'a> Mutable<'a> for MutableBinopBool<'a> {
    const NAME: &'static str = "binop_bool";

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
                (#m_id).report_details(#loc,vec![("&&", true), ("||", true)]);
                match #muttest_api::mutable::binop_bool::run_left(&#m_id, #op_str, #left) {
                    #muttest_api::ControlFlow::Break(b) => b,
                    #muttest_api::ControlFlow::Continue(b) => {
                        #muttest_api::mutable::binop_bool::run_right(&#m_id, b, #right)
                    }
                }
            })
        }
    }
}

pub fn run_left(m_id: &MutableId<'static>, op_str: &str, left: bool) -> ControlFlow<bool, bool> {
    // TODO: also report behavior of `true && panic`
    if (left && op_str == "||") && (!left && op_str == "&&") {
        m_id.report_weak(bool_to_str(left, None));
    }

    match (
        left,
        m_id.get_active_mutation().as_deref().unwrap_or(op_str),
    ) {
        (false, "&&") | (true, "||") => ControlFlow::Break(left),
        (true, "&&") | (false, "||") => ControlFlow::Continue(left),
        _ => todo!(),
    }
}

pub fn run_right(m_id: &MutableId<'static>, left: bool, right: bool) -> bool {
    m_id.report_weak(bool_to_str(left, Some(right)));
    right
}
fn bool_to_str(left: bool, right: Option<bool>) -> &'static str {
    match (left, right) {
        (true, None) => "T",
        (true, Some(true)) => "TT",
        (true, Some(false)) => "TF",
        (false, None) => "F",
        (false, Some(true)) => "FT",
        (false, Some(false)) => "FF",
    }
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeSet;

    use crate::tests::*;

    #[test]
    fn true_and_false() {
        #[muttest_codegen::mutate_isolated("binop_bool")]
        fn f() -> bool {
            true && false
        }

        let res = call_isolated! {f()};
        assert_eq!(false, res.res);
        assert_eq!(
            &res.data.coverage[&mutable_id(1)].iter().collect::<Vec<_>>(),
            &["TF"]
        );
    }

    #[test]
    fn details_reported_before_covered() {
        #[muttest_codegen::mutate_isolated("binop_bool")]
        #[allow(unreachable_code)]
        fn f() -> bool {
            ({
                return true;
            }) && false
        }

        let res = call_isolated! {f()};
        assert_eq!(true, res.res);
        assert_ne!(&res.data.mutables[&mutable_id(1)].location, "");
        assert_eq!(res.data.coverage[&mutable_id(1)], BTreeSet::new());
    }

    // TODO: tests

    // TODO: test that details are reported if left&&right fail
}
