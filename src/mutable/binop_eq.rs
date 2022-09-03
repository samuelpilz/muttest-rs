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
            muttest_api,
            loc,
        } = transformer.new_mutable::<Self>(&op_str, span);

        quote_spanned! {span=>
            ({
                let (left, right) = (#left, #right);
                // for type-inference, keep the original expression in the first branch
                if false {left #op right} else {
                    #muttest_api::mutable::binop_eq::run(&#m_id, #op_str, &left, &right, #loc)
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
    m_id.report_weak(eq_to_str(eq));

    match m_id.get_active_mutation().as_deref().unwrap_or(op_str) {
        "==" => eq,
        "!=" => !eq,
        _ => todo!(),
    }
}

fn eq_to_str(eq: bool) -> &'static str {
    if eq {
        "EQ"
    } else {
        "NE"
    }
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    #[muttest_codegen::mutate_isolated("binop_eq")]
    fn eq_ints() -> bool {
        1 == 2
    }

    #[test]
    fn lt_ints_unchanged_weak_lt() {
        let res = call_isolated!{eq_ints()};
        assert_eq!(false, res.res);
        assert_eq!(
            &res.data.coverage[&mutable_id(1)].iter().collect::<Vec<_>>(),
            &["NE"]
        )
    }

    #[test]
    fn lt_ints_ne() {
        assert_eq!(true, call_isolated!{eq_ints() where 1 => "!="}.res);
    }
}
