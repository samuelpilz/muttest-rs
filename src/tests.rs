use std::{borrow::Cow, collections::BTreeMap, mem, ops::DerefMut, sync::Mutex};

use lazy_static::lazy_static;

use crate::{MutableId, ACTIVE_MUTATION};

static TEST_LOCK: Mutex<()> = Mutex::new(());

lazy_static! {
    pub static ref DETAILS: Mutex<MutableData> = Default::default();
    pub static ref COVERAGE: Mutex<MutableData> = Default::default();
}

type MutableData = BTreeMap<(MutableId<'static>, &'static str), String>;

/// returns a MutableId struct with an id to be used in tests
pub fn mutable_id(id: usize) -> MutableId<'static> {
    MutableId {
        id,
        crate_name: Cow::Borrowed(""),
    }
}

pub struct IsolatedFnCall<T> {
    pub res: T,
    pub details: MutableData,
    pub coverage: MutableData,
}

pub fn without_mutation<T>(action: impl FnOnce() -> T) -> IsolatedFnCall<T> {
    run_mutation(None, action)
}
pub fn with_mutation<T>(
    id: usize,
    mutation: &str,
    action: impl FnOnce() -> T,
) -> IsolatedFnCall<T> {
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
fn run_mutation<T>(
    mutation: Option<(MutableId<'static>, String)>,
    action: impl FnOnce() -> T,
) -> IsolatedFnCall<T> {
    let l = TEST_LOCK.lock();

    let mut details = MutableData::new();
    let mut coverage = MutableData::new();

    // update ACTIVE_MUTATION
    let mut m_map = ACTIVE_MUTATION.write().unwrap();
    // clear other isolated mutations
    m_map.retain(|m_id, _| !m_id.crate_name.is_empty());
    // insert new mutation
    m_map.extend(mutation);
    // release lock on ACTIVE_MUTATION
    std::mem::drop(m_map);

    // insert the new maps
    mem::swap(DETAILS.lock().unwrap().deref_mut(), &mut details);
    mem::swap(COVERAGE.lock().unwrap().deref_mut(), &mut coverage);

    // perform action
    let res = action();

    // get the new maps
    mem::swap(DETAILS.lock().unwrap().deref_mut(), &mut details);
    mem::swap(COVERAGE.lock().unwrap().deref_mut(), &mut coverage);

    // release test lock
    std::mem::drop(l);

    IsolatedFnCall {
        res,
        details,
        coverage,
    }
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

// TODO: restructure these tests to match the structure of mutables & transformer
mod test1;
