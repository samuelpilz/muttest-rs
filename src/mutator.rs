use std::{cmp::Ordering, collections::BTreeSet, fmt, ops::ControlFlow};

use syn::{Arm, Expr, ImplItemFn, ItemFn};

use crate::{
    internal_error,
    report::MutableAnalysis,
    transformer::{Mutated, MuttestTransformer},
    Error,
};

pub mod arm_guard;
pub mod binop_assign;
pub mod binop_bool;
pub mod binop_calc;
pub mod binop_cmp;
pub mod binop_eq;
pub mod extreme;
pub mod lit_bool;
pub mod lit_char;
pub mod lit_int;
pub mod lit_str;
pub mod loop_break;
pub mod unop_calc;

macro_rules! mutables_list {
    ($($m:ident)*) => {
        &[$((stringify!($m), &$m::Mutator)),*]
    };
}

pub const MUTATORS: &[(&str, &'static dyn MutatorFns)] = mutables_list!(
    arm_guard
    binop_assign binop_bool binop_calc binop_cmp binop_eq
    extreme
    lit_bool lit_char lit_int lit_str
    loop_break
    unop_calc
);

/// object-safe set of functions for implementing mutation operators (mutators)
///
/// contain functions for:
/// * proc-macro: source code transformer
/// * runner: valid mutations & weak mutation analysis
#[allow(unused_variables)]
pub trait MutatorFns {
    fn valid_mutations(&self, analysis: &MutableAnalysis) -> Vec<String> {
        analysis
            .mutations
            .iter()
            .flatten()
            .filter_mutable_code(&analysis.code)
    }
    fn identical_behavior(&self, code: &str, behavior: &BTreeSet<String>, mutation: &str) -> bool {
        false
    }
    fn transform_expr(
        &self,
        transformer: &mut MuttestTransformer,
        e: &Expr,
    ) -> ControlFlow<Mutated> {
        ControlFlow::Continue(())
    }
    fn transform_item_fn(
        &self,
        transformer: &mut MuttestTransformer,
        i: &ItemFn,
    ) -> ControlFlow<Mutated> {
        ControlFlow::Continue(())
    }
    fn transform_impl_item_fn(
        &self,
        transformer: &mut MuttestTransformer,
        i: &ImplItemFn,
    ) -> ControlFlow<Mutated> {
        ControlFlow::Continue(())
    }
    fn transform_arm(&self, transformer: &mut MuttestTransformer, a: &Arm) -> ControlFlow<Mutated> {
        ControlFlow::Continue(())
    }
}

fn mutator_for_kind(kind: &str) -> Result<&'static dyn MutatorFns, Error> {
    for &(k, m) in MUTATORS {
        if k == kind {
            return Ok(m);
        }
    }
    Err(Error::UnknownMutableKind(kind.to_owned()))
}

pub fn valid_mutations(kind: &str, analysis: &MutableAnalysis) -> Result<Vec<String>, Error> {
    Ok(mutator_for_kind(kind)?.valid_mutations(analysis))
}

pub fn identical_behavior(
    kind: &str,
    code: &str,
    behavior: &BTreeSet<String>,
    mutation: &str,
) -> Result<bool, Error> {
    Ok(mutator_for_kind(kind)?.identical_behavior(code, behavior, mutation))
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

#[cfg_attr(feature = "selftest", muttest::mutate)]
fn ord_to_str(ord: Option<Ordering>) -> &'static str {
    match ord {
        None => "-",
        Some(Ordering::Less) => "LT",
        Some(Ordering::Equal) => "EQ",
        Some(Ordering::Greater) => "GT",
    }
}
#[cfg_attr(feature = "selftest", muttest::mutate)]
fn ord_from_str(ord: &str) -> Option<Ordering> {
    match ord {
        "-" => None,
        "LT" => Some(Ordering::Less),
        "EQ" => Some(Ordering::Equal),
        "GT" => Some(Ordering::Greater),
        _ => internal_error(Error::InvalidCoverageValue(ord.to_owned())),
    }
}
#[cfg_attr(feature = "selftest", muttest::mutate)]
fn eval_ord(ord: Option<Ordering>, op: &str) -> bool {
    match op {
        "<" => ord.is_some_and(Ordering::is_lt),
        "<=" => ord.is_some_and(Ordering::is_le),
        "==" => ord.is_some_and(Ordering::is_eq),
        "!=" => !ord.is_some_and(Ordering::is_eq),
        ">=" => ord.is_some_and(Ordering::is_ge),
        ">" => ord.is_some_and(Ordering::is_gt),
        // TODO: unit-test this
        m => internal_error(Error::InvalidMutation(Box::leak(
            m.to_owned().into_boxed_str(),
        ))),
    }
}

struct OrdDisplay(Option<Ordering>);
impl fmt::Display for OrdDisplay {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", ord_to_str(self.0))
    }
}
