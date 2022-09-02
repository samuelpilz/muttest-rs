use std::ops::ControlFlow;

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
            muttest_api,
            loc,
        } = transformer.new_mutable::<Self>(&op_str, span);

        quote_spanned! {span=>
            match #muttest_api::mutable::binop_bool::run_left(&#m_id, #op_str, #left, #loc) {
                #muttest_api::ControlFlow::Break(b) => b,
                #muttest_api::ControlFlow::Continue(b) => {
                    #muttest_api::mutable::binop_bool::run_right(&#m_id, b, #right)
                }
            }
        }
    }
}

pub fn run_left(
    m_id: &MutableId<'static>,
    op_str: &str,
    left: bool,
    loc: MutableLocation,
) -> ControlFlow<bool, bool> {
    m_id.report_at(loc);

    // TODO: also report behavior of `true && panic`
    if (left && op_str == "||") && (!left && op_str == "&&") {
        m_id.report_weak(|s| update_weak_str(left, None, s));
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
    m_id.report_weak(|s| update_weak_str(left, Some(right), s));
    right
}

fn update_weak_str(left: bool, right: Option<bool>, s: &str) -> Option<String> {
    if s.is_empty() {
        return Some(bool_to_str(left, right).to_owned());
    }
    let mut opts = parse_coverage(s);

    if opts.contains(&(left, right)) {
        return None;
    }
    opts.push((left, right));
    // TODO: use `iter::intersperse` when stable
    Some(
        opts.into_iter()
            .map(|(l, r)| bool_to_str(l, r))
            .collect::<Vec<_>>()
            .join(":"),
    )
}
pub fn parse_coverage(s: &str) -> Vec<(bool, Option<bool>)> {
    s.split(":").map(|s| bool_from_str(s)).collect::<Vec<_>>()
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
// TODO: report parse errors?
fn bool_from_str(s: &str) -> (bool, Option<bool>) {
    match s {
        "T" => (true, None),
        "F" => (false, None),
        _ => todo!(),
    }
}

#[cfg(test)]
mod tests {
    use crate::tests::mutable_id;


    #[muttest_codegen::mutate_isolated("binop_bool")]
    fn true_and_false() -> bool {
        true && false
    }

    #[test]
    fn true_and_false_unchanged() {
        let res = crate::tests::without_mutation(true_and_false);
        assert_eq!(false, res.res);
        assert_eq!(&res.data.coverage[&mutable_id(1)], "TF");
    }

    // TODO: tests
}
