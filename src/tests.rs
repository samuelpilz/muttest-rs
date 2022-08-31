use std::{borrow::Cow, collections::{BTreeSet, BTreeMap}, mem, sync::Mutex};

use lazy_static::lazy_static;

use crate::{CollectedData, CollectorFile, MutableDataCollector, MutableId, ACTIVE_MUTATION};

static TEST_LOCK: Mutex<()> = Mutex::new(());

lazy_static! {
    pub(crate) static ref DATA_COLLECTOR: MutableDataCollector =
        MutableDataCollector::new_for_test();
}

pub struct IsolatedFnCall<T> {
    pub res: T,
    pub data: CollectedData,
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

    DATA_COLLECTOR.assert_clear();

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

    let data = DATA_COLLECTOR.extract_and_clear();

    // release test lock
    std::mem::drop(l);

    IsolatedFnCall { res, data }
}

/// returns a MutableId struct with an id to be used in tests
pub fn mutable_id(id: usize) -> MutableId<'static> {
    MutableId {
        id,
        crate_name: Cow::Borrowed(""),
    }
}

impl MutableDataCollector {
    fn new_for_test() -> Self {
        MutableDataCollector {
            coverage: Mutex::new(BTreeMap::new()),
            locations: Mutex::new(BTreeSet::new()),
            possible_mutations: Mutex::new(BTreeSet::new()),
            details_file: Mutex::new(CollectorFile::InMemory(
                Self::DETAILS_FILE_HEADER.as_bytes().to_vec(),
            )),
            coverage_file: Mutex::new(CollectorFile::InMemory(
                Self::COVERAGE_FILE_HEADER.as_bytes().to_vec(),
            )),
        }
    }

    fn extract_and_clear(&self) -> CollectedData {
        let mut details_csv = Self::DETAILS_FILE_HEADER.as_bytes().to_vec();
        let mut coverage_csv = Self::COVERAGE_FILE_HEADER.as_bytes().to_vec();

        // lock everything together to ensure isolation
        {
            let mut coverage = self.coverage.lock().unwrap();
            let mut locations = self.locations.lock().unwrap();
            let mut possible_mutations = self.possible_mutations.lock().unwrap();
            let mut details_file = self.details_file.lock().unwrap();
            let mut coverage_file = self.coverage_file.lock().unwrap();

            coverage.clear();
            locations.clear();
            possible_mutations.clear();
            mem::swap(details_file.unwrap_vec_mut(), &mut details_csv);
            mem::swap(coverage_file.unwrap_vec_mut(), &mut coverage_csv);
            // TODO: reset coverage files
        }

        let mut data = CollectedData::default();
        data.read_details_csv(&*details_csv)
            .expect("unable to read csv data");
        data.read_coverage_csv(&*coverage_csv)
            .expect("unable to read csv data");

        data
    }

    fn assert_clear(&self) {
        // lock everything together to ensure isolation
        let coverage = self.coverage.lock().unwrap();
        let locations = self.locations.lock().unwrap();
        let possible_mutations = self.possible_mutations.lock().unwrap();
        let details_file = self.details_file.lock().unwrap();
        let coverage_file = self.coverage_file.lock().unwrap();

        assert!(coverage.is_empty());
        assert!(locations.is_empty());
        assert!(possible_mutations.is_empty());
        assert_eq!(
            &*details_file.unwrap_vec(),
            Self::DETAILS_FILE_HEADER.as_bytes()
        );
        assert_eq!(
            &*coverage_file.unwrap_vec(),
            Self::COVERAGE_FILE_HEADER.as_bytes()
        );
    }
}

impl CollectorFile {
    fn unwrap_vec(&self) -> &[u8] {
        match self {
            CollectorFile::InMemory(v) => v,
            _ => panic!("expect in-memory file"),
        }
    }
    fn unwrap_vec_mut(&mut self) -> &mut Vec<u8> {
        match self {
            CollectorFile::InMemory(v) => v,
            _ => panic!("expect in-memory file"),
        }
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
