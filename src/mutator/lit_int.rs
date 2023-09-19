use std::{fmt, ops::ControlFlow};

use quote::quote_spanned;
use syn::{Expr, ExprLit, Lit};

use crate::{
    internal_error,
    report::{MutableAnalysis, MutableId},
    transformer::{Mutated, MuttestTransformer, TransformSnippets},
    Error,
};

use super::MutatorFns;

pub struct Mutator;

#[cfg_attr(feature = "selftest", muttest::mutate)]
impl MutatorFns for Mutator {
    fn valid_mutations(&self, analysis: &MutableAnalysis) -> Vec<String> {
        let Ok(i) = analysis.code.parse::<u128>() else {
            return vec![];
        };
        let mut m = vec![];
        let (signed, max) = range_for_type(analysis.ty.as_deref());
        if i > 0 {
            // mutate to i-1 less, if possible
            m.push((i - 1).to_string());

            // also mutate to 0
            if i > 1 {
                m.push("0".to_owned());
            }
        }
        if signed {
            // negative range is 1 larger than positive range, so negating is always valid
            m.push((-(i128::try_from(i).unwrap())).to_string());
        }
        if max > i {
            // mutation i+1, if allowed by the range
            m.push((i + 1).to_string())
        }
        m
    }

    #[cfg_attr(muttest, ignore)]
    fn transform_expr(
        &self,
        transformer: &mut MuttestTransformer,
        e: &Expr,
    ) -> ControlFlow<Mutated> {
        let Expr::Lit(ExprLit {
            lit: Lit::Int(lit), ..
        }) = e
        else {
            return ControlFlow::Continue(());
        };
        let span = lit.span();

        let TransformSnippets { m_id, muttest_api } = transformer.new_mutable(span);

        ControlFlow::Break(Mutated {
            transformed: quote_spanned! {span=>
                #muttest_api::mutator::lit_int::run(#m_id, #lit)
            },
            code: lit.base10_digits().to_string(),
            span,
            type_info: Some(lit.suffix().to_owned()).filter(|s| !s.is_empty()),
        })
    }
}

#[cfg_attr(feature = "selftest", muttest::mutate)]
pub fn run<I: MutableInt>(m_id: MutableId, lit: I) -> I {
    m_id.write_coverage();
    m_id.write_types(I::type_str(), "");

    match m_id.get_action() {
        None => lit,
        Some(m) => {
            let Some(i) = I::parse(m) else {
                internal_error(Error::InvalidMutation(m))
            };
            i
        }
    }
}

pub trait MutableInt: Copy + fmt::Display {
    fn type_str() -> &'static str;
    fn parse(s: &str) -> Option<Self>;
}
macro_rules! mutable_ints {
    ($($t:ident),*) => {
        $(impl MutableInt for $t {
            fn type_str() -> &'static str {
                stringify!($t)
            }
            fn parse(s: &str) -> Option<Self> {
                s.parse().ok()
            }
        })*
        const RANGES: &[(&str, bool, u128)] = &[
            $((stringify!($t), $t::MIN != 0, $t::MAX.abs_diff(0) as _)),*
        ];
    };
}
mutable_ints!(u8, u16, u32, u64, u128, usize, i8, i16, i32, i64, i128, isize);

fn range_for_type(t: Option<&str>) -> (bool, u128) {
    match t {
        None => (false, i8::MAX.try_into().unwrap()),
        Some(t) => {
            for &(t1, signed, max) in RANGES {
                if t == t1 {
                    return (signed, max);
                }
            }
            internal_error(Error::InvalidType(t.to_owned()))
        }
    }
}

#[cfg(test)]
#[cfg_attr(feature = "selftest", muttest::tests)]
mod tests {
    use super::Mutator;
    use crate::{mutator::MutatorFns, tests::*};

    #[test]
    fn two_usize() {
        #[muttest_codegen::mutate_isolated("lit_int")]
        fn f() -> usize {
            2
        }

        let data = data_isolated!(f);
        assert_eq!(data.mutables.len(), 1);

        assert_eq!(call_isolated! {f()}.res, 2);
        assert_eq!(call_isolated! {f() where 1: "1"}.res, 1);
        assert_eq!(call_isolated! {f() where 1: "4"}.res, 4);
    }

    #[test]
    fn consts_not_mutated() {
        #[muttest_codegen::mutate_isolated("lit_int")]
        fn _f() {
            const X1: u32 = 1;
            static X2: u8 = 1;
            const fn plus1(x: u8) -> u8 {
                x + 1
            }
        }
        let report = data_isolated!(_f);
        assert_eq!(report.mutables.len(), 0);
    }
    #[test]
    fn const_generics_not_mutated() {
        #[muttest_codegen::mutate_isolated("lit_int")]
        fn _f(_: [usize; 2]) -> [i128; 2] {
            [1; 2]
        }
        let report = data_isolated!(_f);
        assert_eq!(report.mutables.len(), 1);
        assert_eq!(report.for_mutable(1).analysis.code, "1");
    }

    #[test]
    fn patterns_not_mutated() {
        #[muttest_codegen::mutate_isolated("lit_int")]
        fn _f(x: isize) {
            match x {
                1 => {}
                3..=4 => {}
                _ => {}
            }
        }
        let data = data_isolated!(_f);
        assert_eq!(data.mutables.len(), 0);
    }
    #[test]
    fn tuple_index_not_mutated() {
        #[muttest_codegen::mutate_isolated("lit_int")]
        fn _f() {
            ("", "").1;
        }
        let data = data_isolated!(_f);
        assert_eq!(data.mutables.len(), 0);
    }
    #[test]
    fn enum_discriminant_not_mutated() {
        #[muttest_codegen::mutate_isolated("lit_int")]
        fn _f() {
            enum X {
                A = 1,
                B = 2,
            }
        }
        let data = data_isolated!(_f);
        assert_eq!(data.mutables.len(), 0);
    }

    #[test]
    fn typed_literals_mutated_without_coverage() {
        #[muttest_codegen::mutate_isolated("lit_int")]
        fn _f() {
            1u32;
            2i32;
        }
        let data = data_isolated!(_f);
        assert_eq!(data.mutables.len(), 2);
        assert_eq!(
            Mutator
                .valid_mutations(&data.for_mutable(1).analysis)
                .to_vec_deref()
                .sorted(),
            vec!["0", "2"].sorted()
        );
        assert_eq!(
            Mutator
                .valid_mutations(&data.for_mutable(2).analysis)
                .to_vec_deref()
                .sorted(),
            vec!["-2", "0", "1", "3"].sorted()
        );
    }
}
