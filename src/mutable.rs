use std::io::Write;

use proc_macro2::{Span, TokenStream};

use crate::{transformer::MuttestTransformer, MutableData};

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

pub const MUTABLE_DEFINITIONS_CSV_HEAD: &str = "id,kind,code,file,path,attr_span,span";

pub trait Mutable<'a> {
    const NAME: &'static str;

    fn span(&self) -> Span;

    fn transform<W: Write>(self, transformer: &mut MuttestTransformer<W>) -> TokenStream;
}

// TODO: for many mutables, the possible mutations should be clear from context
pub fn mutations_for_mutable(mutable: &MutableData) -> Option<Vec<String>> {
    Some(match &*mutable.kind {
        lit_int::MutableLitInt::NAME => {
            let i = mutable.code.parse::<u128>().expect("unable to parse int");
            let mut m = vec![];
            if i != 0 {
                m.push((i - 1).to_string());
            }
            m.push((i + 1).to_string());
            m
        }
        lit_str::MutableLitStr::NAME => {
            if mutable.code.is_empty() {
                vec![]
            } else {
                vec![String::new()]
            }
        }
        // fallback to mutable's description of possible mutations
        _ => mutable
            .details
            .iter()
            .flat_map(|m| &m.possible_mutations)
            .filter(|&x| x != &mutable.code)
            .map(ToOwned::to_owned)
            .collect(),
    })
}

// TODO: tests
