use proc_macro2::{Span, TokenStream};

use crate::{report::MutableAnalysis, transformer::MuttestTransformer, Error};

pub mod assign_op;
pub mod binop_bool;
pub mod binop_calc;
pub mod binop_cmp;
pub mod binop_eq;
pub mod extreme;
pub mod lit_int;
pub mod lit_str;
// TODO:
// * lits: char, byte, byte_str, float
// * unop: neg, not

pub trait Mutable<'a> {
    const NAME: &'static str;

    fn span(&self) -> Span;

    fn transform(self, transformer: &mut MuttestTransformer) -> TokenStream;
}

// TODO: for many mutables, the possible mutations should be clear from definition
// TODO: tests
#[cfg_attr(test, muttest_codegen::mutate_selftest)]
pub fn mutations_for_mutable(mutable: &MutableAnalysis) -> Result<Vec<String>, Error> {
    Ok(match &*mutable.kind {
        lit_int::MutableLitInt::NAME => {
            let i = mutable.code.parse::<u128>().expect("unable to parse int");
            let mut m = vec![];
            if i != 0 {
                m.push((i - 1).to_string());
            }
            // TODO: type-sensitive detection of max
            m.push((i + 1).to_string());
            m
        }
        lit_str::MutableLitStr::NAME => {
            if mutable.code == "" {
                vec![]
            } else {
                vec![r#""""#.to_owned()]
            }
        }
        binop_cmp::MutableBinopCmp::NAME => ["<", "<=", ">=", ">"]
            .iter()
            .copied()
            .filter(|&x| x != &mutable.code)
            .map(ToOwned::to_owned)
            .collect(),
        binop_eq::MutableBinopEq::NAME => ["==", "!="]
            .iter()
            .copied()
            .filter(|&x| x != &mutable.code)
            .map(ToOwned::to_owned)
            .collect(),
        binop_bool::MutableBinopBool::NAME => ["&&", "||"]
            .iter()
            .copied()
            .filter(|&x| x != &mutable.code)
            .map(ToOwned::to_owned)
            .collect(),
        // fallback to mutable's description of possible mutations
        _ => mutable
            .mutations
            .iter()
            .flatten()
            .filter(|&x| x != &mutable.code)
            .map(ToOwned::to_owned)
            .collect(),
    })
}
