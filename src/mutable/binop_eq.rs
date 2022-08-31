use proc_macro2::{Span, TokenStream};
use quote::{quote_spanned, ToTokens};

use crate::{
    transformer::{Mutable, MuttestTransformer, TransformSnippets},
    *,
};

pub struct MutableBinopEq<'a> {
    pub left: &'a dyn ToTokens,
    pub right: &'a dyn ToTokens,
    pub op: &'a dyn ToTokens,
    pub span: Span,
}

impl<'a> Mutable<'a> for MutableBinopEq<'a> {
    const NAME: &'static str = "binop_eq";

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
                    #core_crate::mutable::binop_eq::run(&#m_id, #op_str, &left, &right, #loc)
                }
            },).0
        }
    }
}

pub fn run<T: PartialEq<T1>, T1>(
    m_id: &MutableId<'static>,
    op_str: &str,
    left: &T,
    right: &T1,
    loc: MutableLocation,
) -> bool {
    m_id.report_at(loc);

    let eq = left.eq(right);
    // this reports behavior but is irrelevant for weak mutation testing
    m_id.report_weak(|s| update_weak_str(eq, s));

    match m_id.get_active_mutation().as_deref().unwrap_or(op_str) {
        "==" => eq,
        "!=" => !eq,
        _ => todo!(),
    }
}

fn update_weak_str(eq: bool, s: &str) -> Option<String> {
    if s.is_empty() {
        return Some(eq_to_str(eq).to_owned());
    }
    let mut opts = parse_coverage(s);

    if opts.contains(&eq) {
        return None;
    }
    opts.push(eq);
    // TODO: use `iter::intersperse` when stable
    Some(
        opts.into_iter()
            .map(eq_to_str)
            .collect::<Vec<_>>()
            .join(":"),
    )
}
pub fn parse_coverage(s: &str) -> Vec<bool> {
    s.split(":").map(|s| eq_from_str(s)).collect::<Vec<_>>()
}
fn eq_to_str(eq: bool) -> &'static str {
    if eq {
        "EQ"
    } else {
        "NE"
    }
}
// TODO: report parse errors?
fn eq_from_str(s: &str) -> bool {
    match s {
        "EQ" => true,
        "NE" => false,
        _ => todo!(),
    }
}

#[cfg(test)]
mod tests {
    use crate::tests::mutable_id;

    #[muttest_codegen::mutate_isolated("binop_eq")]
    fn eq_ints() -> bool {
        1 == 2
    }

    #[test]
    fn lt_ints_unchanged_weak_lt() {
        let res = crate::tests::without_mutation(eq_ints);
        assert_eq!(false, res.res);
        assert_eq!(&res.data.coverage[&mutable_id(1)], "NE")
    }

    #[test]
    fn lt_ints_ne() {
        assert_eq!(true, crate::tests::with_mutation(1, "!=", eq_ints).res);
    }
}