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

    fn span(&self) -> Span {
        self.span
    }

    fn transform(self, transformer: &mut MuttestTransformer) -> TokenStream {
        let span = self.span;
        let lit = self.lit;

        let TransformSnippets {
            m_id,
            muttest_api,
            loc,
        } = transformer.new_mutable(&self, &self.base10_digits);
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
    use crate::tests::*;

    #[test]
    fn two_usize() {
        #[muttest_codegen::mutate_isolated("lit_int")]
        fn f() -> usize {
            2
        }

        let data = data_isolated!(f);
        assert_eq!(data.mutables.len(), 1);

        assert_eq!(call_isolated! {f()}.res, 2);
        assert_eq!(call_isolated! {f() where 1 => "1"}.res, 1);
        assert_eq!(call_isolated! {f() where 1 => "4"}.res, 4);
    }

    // TODO: also test invalid mutations

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
        let data = data_isolated!(_f);
        assert_eq!(data.mutables.len(), 0);
    }
    #[test]
    fn const_generics_not_mutated() {
        #[muttest_codegen::mutate_isolated("lit_int")]
        fn _f(_: [usize; 2]) -> [i128; 2] {
            [1; 2]
        }
        let data = data_isolated!(_f);
        assert_eq!(data.mutables.len(), 1);
        assert_eq!(data.mutables[&mutable_id(1)].code, "1");
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
}
