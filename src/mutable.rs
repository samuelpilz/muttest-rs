use proc_macro2::{Span, TokenStream};

use crate::{report::MutableAnalysis, transformer::MuttestTransformer, Error};

pub mod assign_op;
pub mod binop_bool;
pub mod binop_calc;
pub mod binop_cmp;
pub mod binop_eq;
pub mod extreme;
pub mod lit_char;
pub mod lit_int;
pub mod lit_str;
// TODO:
// * lits: byte, byte_str, float
// * unop: neg, not

/// a general trait for mutables with shared methods.
///
/// consists of:
/// * for proc-macro: source code transformer
/// * for runner: possible mutations & weak mutation analysis
pub trait Mutable<'a> {
    const NAME: &'static str;

    fn span(&self) -> Span;

    fn transform(self, transformer: &mut MuttestTransformer) -> TokenStream;

    fn mutations(analysis: &MutableAnalysis) -> Vec<String> {
        analysis
            .mutations
            .iter()
            .flatten()
            .filter_mutable_code(&analysis.code)
    }

    #[allow(unused_variables)]
    fn identical_behavior(analysis: &MutableAnalysis, mutation: &str) -> bool {
        false
    }
}

pub fn mutations_for_mutable(kind: &str, analysis: &MutableAnalysis) -> Result<Vec<String>, Error> {
    macro_rules! match_mutables {
        ($($m:ident,)*) => {
            match kind {
                $($m::Mutable::NAME => $m::Mutable::mutations(analysis),)*
                _ => return Err(Error::UnknownMutableKind(kind.to_owned())),
            }
        };
    }
    Ok(match_mutables!(
        assign_op, binop_bool, binop_calc, binop_cmp, binop_eq, extreme, lit_char, lit_int,
        lit_str,
    ))
}

pub fn identical_behavior_for_mutable(
    kind: &str,
    analysis: &MutableAnalysis,
    mutation: &str,
) -> Result<bool, Error> {
    macro_rules! match_mutables {
        ($($m:ident,)*) => {
            match kind {
                $($m::Mutable::NAME => $m::Mutable::identical_behavior(analysis, mutation),)*
                _ => return Err(Error::UnknownMutableKind(kind.to_owned())),
            }
        };
    }
    Ok(match_mutables!(
        assign_op, binop_bool, binop_calc, binop_cmp, binop_eq, extreme, lit_char, lit_int,
        lit_str,
    ))
}

trait FilterMutableCode {
    fn filter_mutable_code(self, code: &str) -> Vec<String>;
}
impl<'a, S: AsRef<str> + 'a, I: IntoIterator<Item = S> + 'a> FilterMutableCode for I {
    fn filter_mutable_code(self, code: &str) -> Vec<String> {
        self.into_iter()
            .filter(|m| m.as_ref() != code)
            .map(|m| m.as_ref().to_owned())
            .collect()
    }
}
