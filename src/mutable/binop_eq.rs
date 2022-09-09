use proc_macro2::{Span, TokenStream};
use quote::{quote_spanned, ToTokens};

use crate::MutableId;

use super::{Mutable, MuttestTransformer, TransformSnippets};

pub struct MutableBinopEq<'a> {
    pub left: &'a dyn ToTokens,
    pub right: &'a dyn ToTokens,
    pub op: &'a dyn ToTokens,
    pub span: Span,
}

impl<'a> Mutable<'a> for MutableBinopEq<'a> {
    const NAME: &'static str = "binop_eq";

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
                (#m_id).report_details(#loc,"","==:!=");
                let (_left, _right) = (&#left, &#right);
                // for type-inference, keep the original expression in the first branch
                if false {_left #op _right} else {
                    #muttest_api::mutable::binop_eq::run(&#m_id, #op_str, &_left, &_right)
                }
            })
        }
    }
}

#[cfg_attr(test, muttest_codegen::mutate_selftest)]
pub fn run<T: PartialEq<T1>, T1>(
    m_id: &MutableId<'static>,
    op_str: &str,
    left: &T,
    right: &T1,
) -> bool {
    let eq = left.eq(right);
    // this reports behavior but is irrelevant for weak mutation testing
    m_id.report_weak(eq_to_str(eq));

    match m_id.get_active_mutation().as_deref().unwrap_or(op_str) {
        "==" => eq,
        "!=" => !eq,
        _ => todo!(),
    }
}

#[cfg_attr(test, muttest_codegen::mutate_selftest)]
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

    #[test]
    fn eq_ints() {
        #[muttest_codegen::mutate_isolated("binop_eq")]
        fn f() -> bool {
            1 == 2
        }
        let res = call_isolated! {f()};
        assert_eq!(false, res.res);
        assert_eq!(
            &res.data.coverage[&mutable_id(1)].iter().collect::<Vec<_>>(),
            &["NE"]
        );
        assert_eq!(true, call_isolated! {f() where 1 => "!="}.res);
    }

    #[test]
    fn compare_unsized() {
        #[muttest_codegen::mutate_isolated("binop_eq")]
        fn f(s: String) -> bool {
            *s == *"2";
            // second call to ensure that s is not moved in call before
            *s == *"2"
        }

        assert_eq!(call_isolated! {f(String::new())}.res, false);
        assert_eq!(call_isolated! {f(String::new()) where 2 => "!="}.res, true);
    }

    #[test]
    fn compare_tmp_vars() {
        #[muttest_codegen::mutate_isolated("binop_eq")]
        fn f() -> bool {
            vec!["1", "2", "3"].into_iter().collect::<String>() == "123".to_owned()
        }

        assert_eq!(call_isolated! {f()}.res, true);
        assert_eq!(call_isolated! {f() where 1 => "!="}.res, false);
    }

    #[test]
    fn details_reported_before_covered() {
        #[muttest_codegen::mutate_isolated("binop_eq")]
        #[allow(unreachable_code)]
        fn f() -> u8 {
            return loop {
                if ({
                    break 1;
                    2
                }) == 2
                {
                    return 3;
                } else {
                    return 4;
                };
            };
        }

        let res = call_isolated! {f()};
        assert_eq!(1, res.res);
        assert_ne!(res.data.mutables[&mutable_id(1)].details, None);
        assert_eq!(res.data.coverage.get(&mutable_id(1)), None);
    }
}
