use proc_macro2::{Span, TokenStream};
use quote::{quote_spanned, ToTokens};

use crate::{
    transformer::{Mutable, MuttestTransformer, TransformSnippets},
    *,
};

pub struct MutableLitInt<'a> {
    pub base10_digits: &'a str,
    pub span: Span,
    pub lit: &'a dyn ToTokens,
}

impl<'a> Mutable<'a> for MutableLitInt<'a> {
    const NAME: &'static str = "lit_int";

    fn transform(self, transformer: &mut MuttestTransformer) -> TokenStream {
        let span = self.span;
        let lit = self.lit;

        let TransformSnippets {
            m_id,
            muttest_api,
            loc,
        } = transformer.new_mutable::<Self>(&self.base10_digits, span);
        quote_spanned! {span=>
            #muttest_api::mutable::lit_int::run(&#m_id, #lit, #loc)
        }
    }
}

pub fn run<T: MutableInt>(m_id: &MutableId<'static>, x: T, loc: MutableLocation) -> T {
    m_id.report_at(loc);

    match m_id.get_active_mutation().as_deref() {
        None => x,
        Some(p) if p.chars().all(|c| c.is_numeric()) => T::parse(p),
        _ => todo!(), // TODO: panic and report
    }
}

pub trait MutableInt: Copy + fmt::Display {
    fn type_str() -> &'static str;
    fn parse(s: &str) -> Self;
}
macro_rules! mutable_ints {
    ($($t:ty),*) => {
        $(impl MutableInt for $t {
            fn type_str() -> &'static str {
                stringify!($t)
            }
            fn parse(s: &str) -> Self {
                s.parse().expect("unable to parse number")
            }
        })*
    };
}
mutable_ints!(u8, u16, u32, u64, u128, usize, i8, i16, i32, i64, i128, isize);

#[cfg(test)]
mod tests {
    #[muttest_codegen::mutate_isolated("lit_int")]
    fn two_usize() -> usize {
        2
    }

    #[test]
    fn two_usize_mutables() {
        assert_eq!(two_usize::NUM_MUTABLES, 1);
        assert_eq!(two_usize::MUTABLES_CSV.lines().count(), 2);
    }

    #[test]
    fn two_usize_unchanged() {
        assert_eq!(crate::tests::without_mutation(two_usize).res, 2);
    }

    #[test]
    fn two_usize_1() {
        assert_eq!(crate::tests::with_mutation(1, "1", two_usize).res, 1);
    }

    #[test]
    fn two_usize_4() {
        assert_eq!(crate::tests::with_mutation(1, "4", two_usize).res, 4);
    }

    // TODO: also test invalid mutations
}
