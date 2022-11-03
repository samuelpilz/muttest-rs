use std::{
    cell::RefCell,
    collections::{BTreeMap, BTreeSet},
    ops::Deref,
    sync::{Arc, Mutex, RwLock},
};

use crate::{
    context::{
        CollectedData, DataCollector, MuttestContext, COVERAGE_FILE_CSV_HEAD, DETAILS_FILE_CSV_HEAD,
    },
    BakedMutableId, MutableId,
};

pub use crate::{call_isolated, data_isolated, return_early_if_nesting};

// TODO: BTreeMap::new is not yet const :/
lazy_static::lazy_static! {
    static ref TEST_CONTEXT: RwLock<BTreeMap<String, Arc<TestContext>>> =
        RwLock::new(BTreeMap::new());
}

thread_local! {
    /// describes the nesting in the testcase for selftest
    pub static MUTATION_NESTING: RefCell<Vec<&'static str>> = RefCell::new(vec![]);
}

pub enum NestingToken {
    Isolated,
    Selftest(&'static str),
    Nested,
}

impl NestingToken {
    pub fn create(m_id: BakedMutableId, s: &'static str) -> Self {
        // if this is compiled in `cfg(test)`, then only possible mutations are isolated and selftest
        if m_id.is_isolated() {
            return Self::Isolated;
        }
        assert_eq!(m_id.crate_name, "muttest-core:muttest_core");

        MUTATION_NESTING.with(move |v| {
            let mut v = v.borrow_mut();
            if v.contains(&s) {
                Self::Nested
            } else {
                v.push(s);
                Self::Selftest(s)
            }
        })
    }
}
impl Drop for NestingToken {
    fn drop(&mut self) {
        match self {
            Self::Selftest(s) => MUTATION_NESTING.with(|v| {
                let last = v.borrow_mut().pop().expect("nesting is empty");
                assert_eq!(last, *s, "invalid nesting");
            }),
            _ => {}
        }
    }
}
#[macro_export]
macro_rules! return_early_if_nesting {
    ($m_id:expr, $name:expr, $e:expr) => {
        let __muttest_nesting_token = match crate::tests::NestingToken::create($m_id, $name) {
            crate::tests::NestingToken::Nested => return $e,
            t => t,
        };
    };
}

type TestContext = MuttestContext<Vec<u8>>;

impl BakedMutableId {
    pub(crate) fn is_isolated(self) -> bool {
        self.crate_name.starts_with("#")
    }
    pub(crate) fn test_context(self) -> Arc<TestContext> {
        TEST_CONTEXT
            .read()
            .unwrap()
            .get(self.crate_name)
            .expect("mutation for this testcase not set")
            .clone()
    }
}

#[macro_export]
macro_rules! call_isolated {
    ($f:ident $(::<$t:ty>)? ($($args:expr),*) $(where $m_id:expr => $m:expr)?) => {
        crate::tests::run$(::<$t>)?(
            $f::MUTABLES_CSV,
            $f::NUM_MUTABLES,
            $f::TARGET_NAME,
            || $f($($args),*),
            vec![$(($m_id, $m))?]
        )
    };
}
#[macro_export]
macro_rules! data_isolated {
    ($f:ident) => {
        crate::context::CollectedData::from_defs_checked($f::NUM_MUTABLES, $f::MUTABLES_CSV)
    };
}

pub struct IsolatedFnCall<T> {
    pub res: T,
    pub data: CollectedData,
}

pub fn run<'a, T>(
    defs_csv: &str,
    num_mutables: usize,
    target_name: &str,
    action: impl FnOnce() -> T,
    mutation: Vec<(usize, &'a str)>,
) -> IsolatedFnCall<T> {
    let mut data = CollectedData::from_defs_checked(num_mutables, defs_csv);
    for (id, _) in &mutation {
        if !data.mutables.contains_key(&id) {
            eprintln!("mutation {mutation:?}");
            eprintln!("data:");
            eprintln!("{defs_csv}");
            panic!("mutable id {id} is not a valid mutable id")
        }
    }

    let context = TestContext {
        target: target_name.to_owned(),
        mutations: mutation
            .into_iter()
            .map(|(m_id, m)| (m_id, Arc::from(m)))
            .collect(),
        collector: DataCollector::new_for_test(),
    };
    TEST_CONTEXT
        .write()
        .unwrap()
        .insert(target_name.to_owned(), Arc::new(context))
        .ok_or(())
        .expect_err("concurrent execution of test ");

    // perform action
    let res = action();

    // extract data
    let context = TEST_CONTEXT.write().unwrap().remove(target_name).unwrap();
    let context = Arc::try_unwrap(context)
        .ok()
        .expect("someone still has a reference to this context");
    context.collector.extract_data(&mut data);

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

impl DataCollector<Vec<u8>> {
    fn new_for_test() -> Self {
        DataCollector {
            details: Mutex::new(BTreeSet::new()),
            coverage: Mutex::new(BTreeMap::new()),
            details_file: Some(Mutex::new(DETAILS_FILE_CSV_HEAD.as_bytes().to_vec())),
            coverage_file: Some(Mutex::new(COVERAGE_FILE_CSV_HEAD.as_bytes().to_vec())),
        }
    }

    fn extract_data(self, data: &mut CollectedData) {
        data.read_details_csv(&*self.details_file.unwrap().into_inner().unwrap())
            .expect("unable to read csv data");
        data.read_coverage_csv(&*self.coverage_file.unwrap().into_inner().unwrap())
            .expect("unable to read csv data");
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
