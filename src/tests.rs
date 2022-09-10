use std::{
    borrow::Cow,
    collections::{BTreeMap, BTreeSet},
    mem,
    ops::Deref,
    sync::{Arc, Mutex},
};

use lazy_static::lazy_static;

use crate::{
    collector::{CollectedData, CollectorFile},
    DataCollector, MutableId, ACTIVE_MUTATION,
};

pub use crate::{call_isolated, data_isolated};

lazy_static! {
    pub(crate) static ref DATA_COLLECTOR: DataCollector = DataCollector::new_for_test();
}

#[macro_export]
macro_rules! call_isolated {
    ($f:ident $(::<$t:ty>)? ($($args:expr),*) $(where $m_id:expr => $m:expr)?) => {
        crate::tests::run$(::<$t>)?(
            $f::MUTABLES_CSV,
            $f::NUM_MUTABLES,
            || $f($($args),*),
            vec![$(($m_id, $m))?]
        )
    };
}
#[macro_export]
macro_rules! data_isolated {
    ($f:ident) => {
        crate::collector::CollectedData::from_defs($f::NUM_MUTABLES, $f::MUTABLES_CSV)
    };
}

pub struct IsolatedFnCall<T> {
    pub res: T,
    pub data: CollectedData,
}

static TEST_LOCK: Mutex<()> = Mutex::new(());

pub fn run<'a, T>(
    defs_csv: &str,
    num_mutables: usize,
    action: impl FnOnce() -> T,
    mutation: Vec<(usize, &'a str)>,
) -> IsolatedFnCall<T> {
    let mut data = CollectedData::from_defs(num_mutables, defs_csv);
    for (m_id, _) in &mutation {
        if !data.mutables.contains_key(&mutable_id(*m_id)) {
            panic!("mutable id {m_id} is not a valid mutable id")
        }
    }

    let l = TEST_LOCK.lock();

    DATA_COLLECTOR.assert_clear();

    // update ACTIVE_MUTATION
    let mut m_map = ACTIVE_MUTATION.write().unwrap();
    // clear other isolated mutations
    m_map.retain(|m_id, _| !m_id.crate_name.is_empty());
    // insert new mutation
    for (m_id, m) in mutation {
        m_map.insert(mutable_id(m_id), Arc::from(m));
    }
    // release lock on ACTIVE_MUTATION
    std::mem::drop(m_map);

    // perform action
    let res = action();

    DATA_COLLECTOR.extract_data_and_clear(&mut data);

    // release test lock
    std::mem::drop(l);

    IsolatedFnCall { res, data }
}

// TODO: make different isolated fns have different `crate_name`s
/// returns a MutableId struct with an id to be used in tests
pub fn mutable_id(id: usize) -> MutableId<'static> {
    MutableId {
        id,
        crate_name: Cow::Borrowed(""),
    }
}

impl CollectedData {
    // TODO: also validate against id collisions?
    pub fn from_defs(num: usize, defs_csv: &str) -> Self {
        let mut cd = Self::new();
        cd.read_definition_csv("", defs_csv.as_bytes()).unwrap();
        for id in cd.mutables.keys() {
            if num < id.id {
                panic!("invalid id {}. max: {num}", id.id);
            }
        }
        cd
    }
}

impl DataCollector {
    fn new_for_test() -> Self {
        DataCollector {
            details: Mutex::new(BTreeSet::new()),
            coverage: Mutex::new(BTreeMap::new()),
            details_file: Mutex::new(CollectorFile::InMemory(
                Self::DETAILS_FILE_HEADER.as_bytes().to_vec(),
            )),
            coverage_file: Mutex::new(CollectorFile::InMemory(
                Self::COVERAGE_FILE_HEADER.as_bytes().to_vec(),
            )),
        }
    }

    fn extract_data_and_clear(&self, data: &mut CollectedData) {
        let mut details_csv = Self::DETAILS_FILE_HEADER.as_bytes().to_vec();
        let mut coverage_csv = Self::COVERAGE_FILE_HEADER.as_bytes().to_vec();

        // lock everything together to ensure isolation
        {
            let mut coverage = self.coverage.lock().unwrap();
            let mut details = self.details.lock().unwrap();
            let mut details_file = self.details_file.lock().unwrap();
            let mut coverage_file = self.coverage_file.lock().unwrap();

            coverage.clear();
            details.clear();
            mem::swap(details_file.unwrap_vec_mut(), &mut details_csv);
            mem::swap(coverage_file.unwrap_vec_mut(), &mut coverage_csv);
        }

        data.read_details_csv(&*details_csv)
            .expect("unable to read csv data");
        data.read_coverage_csv(&*coverage_csv)
            .expect("unable to read csv data");
    }

    fn assert_clear(&self) {
        // lock everything together to ensure isolation
        let coverage = self.coverage.lock().unwrap();
        let details = self.details.lock().unwrap();
        let details_file = self.details_file.lock().unwrap();
        let coverage_file = self.coverage_file.lock().unwrap();

        assert!(coverage.is_empty());
        assert!(details.is_empty());
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

pub const NO_MUTATIONS: &[&str] = &[];

pub trait ToVec<T> {
    fn to_vec(&self) -> Vec<T>
    where
        T: Clone;
    fn to_vec_ref(&self) -> Vec<&T>;
    fn to_vec_deref(&self) -> Vec<&<T as Deref>::Target>
    where
        T: Deref;
    fn to_vec_into<T1>(&self) -> Vec<T1>
    where
        T: Into<T1> + Clone;
}
impl<T> ToVec<T> for BTreeSet<T> {
    fn to_vec(&self) -> Vec<T>
    where
        T: Clone,
    {
        self.iter().cloned().collect()
    }
    fn to_vec_ref(&self) -> Vec<&T> {
        self.iter().collect()
    }
    fn to_vec_deref(&self) -> Vec<&<T as Deref>::Target>
    where
        T: Deref,
    {
        self.iter().map(|x| x.deref()).collect()
    }

    fn to_vec_into<T1>(&self) -> Vec<T1>
    where
        T: Into<T1> + Clone,
    {
        self.iter().cloned().map(|x| x.into()).collect()
    }
}

#[test]
pub fn mutable_id_ord() {
    assert!(
        MutableId {
            crate_name: Cow::Borrowed("a"),
            id: 1,
        } < MutableId {
            crate_name: Cow::Borrowed("b"),
            id: 0,
        }
    )
}
