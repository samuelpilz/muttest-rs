use crate::*;

pub fn mutable_cmp<T: PartialOrd<T1>, T1>(
    m_id: &MutableId,
    op_str: &str,
    left: &T,
    right: &T1,
) -> bool {
    report_coverage(m_id);
    let ord = left.partial_cmp(right);
    // TODO: record behavior for weak mutation testing
    // save_msg(&format!("CMP {m_id}; {ord:?}"));
    if let Some(ord) = ord {
        match get_active_mutation_for_mutable(m_id)
            .as_deref()
            .unwrap_or(op_str)
        {
            "<" => ord.is_lt(),
            "<=" => ord.is_le(),
            ">=" => ord.is_ge(),
            ">" => ord.is_gt(),
            _ => todo!(),
        }
    } else {
        false
    }
}

#[cfg(test)]
mod tests {
    #[muttest_codegen::mutate_isolated("binop_cmp")]
    fn lt_ints() -> bool {
        1 < 2
    }

    #[test]
    fn lt_ints_mutables() {
        assert_eq!(lt_ints::NUM_MUTABLES, 1);
        assert_eq!(lt_ints::MUTABLES_CSV.lines().count(), 2);
    }

    #[test]
    fn lt_ints_unchanged() {
        assert_eq!(true, crate::tests::without_mutation(lt_ints));
    }

    #[test]
    fn lt_ints_gt() {
        assert_eq!(false, crate::tests::with_mutation(1, ">", lt_ints));
    }

    #[test]
    fn lt_ints_ge() {
        assert_eq!(false, crate::tests::with_mutation(1, ">=", lt_ints));
    }
}
