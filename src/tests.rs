use std::{
    collections::{BTreeMap, BTreeSet},
    io::Write,
    mem,
    ops::Deref,
    sync::{Arc, Mutex, RwLock},
};

use lazy_static::lazy_static;

use crate::{
    collector::{CollectedData, COVERAGE_FILE_HEADER, DETAILS_FILE_HEADER},
    BakedMutableId, DataCollector, MutableId, MutationsMap,
};

pub use crate::{call_isolated, data_isolated};

lazy_static! {
    pub(crate) static ref DATA_COLLECTOR: DataCollector<Vec<u8>> = DataCollector::new_for_test();
    // TODO: one MutationsMap per testcase
    pub(crate) static ref TEST_MUTATION: RwLock<Option<MutationsMap>> =
        RwLock::new(None);
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
        crate::collector::CollectedData::from_defs_checked($f::NUM_MUTABLES, $f::MUTABLES_CSV)
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
    let mut data = CollectedData::from_defs_checked(num_mutables, defs_csv);
    for (id, _) in &mutation {
        if !data.mutables.contains_key(&id) {
            panic!("mutable id {id} is not a valid mutable id")
        }
    }

    // TODO: correctly recover from failed tests
    let l = TEST_LOCK.lock();

    DATA_COLLECTOR.clear();

    // setup test mutation
    *TEST_MUTATION.write().unwrap() = Some(
        mutation
            .into_iter()
            .map(|(m_id, m)| (m_id, Arc::from(m)))
            .collect(),
    );

    // perform action
    let res = action();

    // remove test mutation
    *TEST_MUTATION.write().unwrap() = None;

    DATA_COLLECTOR.extract_data_and_clear(&mut data);

    // release test lock
    std::mem::drop(l);

    IsolatedFnCall { res, data }
}

impl CollectedData {
    // TODO: also validate against id collisions?
    pub(crate) fn from_defs_checked(num: usize, defs_csv: &str) -> Self {
        let cd = Self::from_definition_csv(defs_csv.as_bytes()).unwrap();
        for &id in cd.mutables.keys() {
            if num < id {
                panic!("invalid id {id}. max: {num}");
            }
        }
        cd
    }
}

fn csv_headers() -> (Vec<u8>, Vec<u8>) {
    let mut details_csv = vec![];
    writeln!(&mut details_csv, "{DETAILS_FILE_HEADER}").unwrap();
    let mut coverage_csv = vec![];
    writeln!(&mut coverage_csv, "{COVERAGE_FILE_HEADER}").unwrap();

    (details_csv, coverage_csv)
}

impl DataCollector<Vec<u8>> {
    fn new_for_test() -> Self {
        let (details_csv, coverage_csv) = csv_headers();
        DataCollector {
            details: Mutex::new(BTreeSet::new()),
            coverage: Mutex::new(BTreeMap::new()),
            details_file: Some(Mutex::new(details_csv)),
            coverage_file: Some(Mutex::new(coverage_csv)),
        }
    }

    fn extract_data_and_clear(&self, data: &mut CollectedData) {
        let (mut details_csv, mut coverage_csv) = csv_headers();

        // lock everything together to ensure isolation
        {
            let mut coverage = self.coverage.lock().unwrap();
            let mut details = self.details.lock().unwrap();
            let mut details_file = self.details_file.as_ref().unwrap().lock().unwrap();
            let mut coverage_file = self.coverage_file.as_ref().unwrap().lock().unwrap();

            coverage.clear();
            details.clear();
            mem::swap(&mut *details_file, &mut details_csv);
            mem::swap(&mut *coverage_file, &mut coverage_csv);
        }

        data.read_details_csv(&*details_csv)
            .expect("unable to read csv data");
        data.read_coverage_csv(&*coverage_csv)
            .expect("unable to read csv data");
    }

    fn clear(&self) {
        let headers = csv_headers();

        // lock everything together to ensure isolation
        let mut coverage = self.coverage.lock().unwrap();
        let mut details = self.details.lock().unwrap();
        let mut details_file = self.details_file.as_ref().unwrap().lock().unwrap();
        let mut coverage_file = self.coverage_file.as_ref().unwrap().lock().unwrap();

        coverage.clear();
        details.clear();
        *details_file = headers.0;
        *coverage_file = headers.1;
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
        BakedMutableId {
            crate_name: "a",
            id: 1,
        } < BakedMutableId {
            crate_name: "b",
            id: 0,
        }
    );
    assert!(
        MutableId {
            crate_name: "a".to_owned(),
            id: 1,
        } < MutableId {
            crate_name: "b".to_owned(),
            id: 0,
        }
    );
}
