use std::ops::ControlFlow;

use quote::{quote_spanned, ToTokens};
use syn::Arm;

use crate::{
    api::MutableId,
    internal_error,
    report::MutableAnalysis,
    transformer::{Mutated, MuttestTransformer, TransformSnippets},
    Error,
};

use super::MutatorFns;

pub struct Mutator;

#[cfg_attr(feature = "selftest", muttest::mutate)]
impl MutatorFns for Mutator {
    fn valid_mutations(&self, _: &MutableAnalysis) -> Vec<String> {
        vec!["true".to_owned(), "false".to_owned()]
    }

    #[cfg_attr(muttest, ignore)]
    fn transform_arm(&self, transformer: &mut MuttestTransformer, a: &Arm) -> ControlFlow<Mutated> {
        let Some((tok_if, guard)) = &a.guard else {
            return ControlFlow::Continue(());
        };
        let span = tok_if.span;
        let TransformSnippets { m_id, muttest_api } = transformer.new_mutable(span);
        let mut a = a.clone();

        a.guard = Some((
            *tok_if,
            syn::parse2(quote_spanned!(span=> {
                match #muttest_api::mutator::arm_guard::run(#m_id) {
                    #muttest_api::ControlFlow::Continue(_) => #guard,
                    #muttest_api::ControlFlow::Break(b) => b,
                }
            }))
            .unwrap(),
        ));

        ControlFlow::Break(Mutated {
            transformed: a.into_token_stream(),
            code: "if".to_owned(),
            type_info: None,
            span,
        })
    }
}

#[cfg_attr(feature = "selftest", muttest::mutate)]
pub fn run(m_id: MutableId) -> ControlFlow<bool> {
    m_id.write_coverage();

    match m_id.get_action() {
        None => ControlFlow::Continue(()),
        Some("true") => ControlFlow::Break(true),
        Some("false") => ControlFlow::Break(false),
        Some(m) => internal_error(Error::InvalidMutation(m)),
    }
}
#[cfg(test)]
#[cfg_attr(feature = "selftest", muttest::tests)]
mod tests {

    use super::Mutator;
    use crate::{mutator::MutatorFns, tests::*};

    #[test]
    fn guarded_arm() {
        #[muttest_codegen::mutate_isolated("arm_guard")]
        fn f(b: bool) -> i32 {
            match 1 {
                _ if b => 1,
                _ => 2,
            }
        }
        let data = data_isolated!(f);
        assert_eq!(data.mutables.len(), 1);
        assert_eq!(
            Mutator
                .valid_mutations(&data.for_mutable(1).analysis)
                .to_vec_deref()
                .sorted(),
            vec!["true", "false"].sorted()
        );

        assert_eq!(call_isolated! {f(true)}.res, 1);
        assert_eq!(call_isolated! {f(false)}.res, 2);
        assert_eq!(call_isolated! {f(false) where 1: "true"}.res, 1);
        assert_eq!(call_isolated! {f(true) where 1: "false"}.res, 2);
    }
}
