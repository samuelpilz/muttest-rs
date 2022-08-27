use std::{borrow::Cow, sync::Mutex};

use crate::{MutableId, ACTIVE_MUTATION};

static TEST_LOCK: Mutex<()> = Mutex::new(());

pub fn without_mutation<T>(action: impl Fn() -> T) -> T {
    run_mutation(None, action)
}
pub fn with_mutation<T>(id: usize, mutation: &str, action: impl Fn() -> T) -> T {
    run_mutation(
        Some((
            MutableId {
                id,
                crate_name: Cow::Borrowed(""),
            },
            mutation.to_owned(),
        )),
        action,
    )
}

// TODO: gather mutable-details and coverage data
fn run_mutation<T>(mutation: Option<(MutableId<'static>, String)>, action: impl Fn() -> T) -> T {
    let l = TEST_LOCK.lock();

    // update ACTIVE_MUTATION
    let mut m_map = ACTIVE_MUTATION.write().unwrap();
    // clear other isolated mutations
    m_map.retain(|m_id, _| !m_id.crate_name.is_empty());
    // insert new mutation
    m_map.extend(mutation);
    // release lock on ACTIVE_MUTATION
    std::mem::drop(m_map);

    // perform action
    let res = action();

    // release test lock
    std::mem::drop(l);
    res
}

// this shows how a mutation testing for the `muttest-core` lib itself could work in theory
#[muttest_codegen::mutate_selftest]
fn example_selftest_fn() -> usize {
    1
}

#[test]
fn example_selftest_test() {
    assert_eq!(example_selftest_fn(), 1);
}

// TODO: maybe restructure these tests to match the structure of mutables & transformer
mod speculative;
mod test1;
