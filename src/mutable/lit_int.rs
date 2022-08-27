use crate::*;

pub fn mutable_int<T: MutableInt>(m_id: &MutableId<'static>, x: T) -> T {
    report_coverage(m_id);
    report_mutable_type(m_id, T::type_str());
    match get_active_mutation_for_mutable(m_id).as_deref() {
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
        assert_eq!(crate::tests::without_mutation(two_usize), 2);
    }

    #[test]
    fn two_usize_1() {
        assert_eq!(crate::tests::with_mutation(1, "1", two_usize), 1);
    }

    #[test]
    fn two_usize_4() {
        assert_eq!(crate::tests::with_mutation(1, "4", two_usize), 4);
    }

    // TODO: also test invalid mutations
}
