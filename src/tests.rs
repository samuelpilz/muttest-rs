use std::{
    cell::RefCell,
    collections::{BTreeMap, BTreeSet},
    ops::Deref,
    sync::{Arc, Mutex, RwLock},
};

use crate::{
    context::{IMuttestContext, MuttestContext, COVERAGE_FILE_CSV_HEAD, DETAILS_FILE_CSV_HEAD},
    mutable_id::CrateId,
    report::{MutableAnalysis, MuttestReportForCrate},
    BakedLocation, BakedMutableId, CrateLocalMutableId, MutableId, Mutation,
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
        // this is compiled in `cfg(test)`, then only possible mutations are isolated and selftest
        if m_id.is_isolated() {
            return Self::Isolated;
        }
        assert_eq!(m_id.pkg_name, "muttest-core");
        assert_eq!(m_id.crate_name, "muttest_core");

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
        self.pkg_name == "#isolated"
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
impl MuttestReportForCrate {
    pub(crate) fn analysis(&self, id: usize) -> &MutableAnalysis {
        &self
            .mutables
            .iter()
            .find(|(m_id, _)| m_id.id == id)
            .unwrap_or_else(|| panic!("no mutable wih {id} found"))
            .1
            .analysis
    }
}

#[macro_export]
macro_rules! call_isolated {
    ($f:ident $(::<$t:ty>)? ($($args:expr),*) $(where $m_id:expr => $m:expr)?) => {
        crate::tests::run$(::<$t>)?(
            $f::PKG_NAME,
            $f::CRATE_NAME,
            $f::NUM_MUTABLES,
            $f::MUTABLES_CSV,
            || $f($($args),*),
            vec![$(($m_id, $m))?]
        )
    };
}
#[macro_export]
macro_rules! data_isolated {
    ($f:ident) => {{
        eprintln!("{}:{}", $f::PKG_NAME, $f::CRATE_NAME);
        eprintln!("{}", $f::MUTABLES_CSV);
        crate::report::MuttestReportForCrate::from_defs_checked($f::NUM_MUTABLES, $f::MUTABLES_CSV)
    }};
}

pub struct IsolatedFnCall<T> {
    pub res: T,
    pub report: MuttestReportForCrate,
}

pub fn run<'a, T>(
    pkg_name: &str,
    crate_name: &str,
    num_mutables: usize,
    defs_csv: &str,
    action: impl FnOnce() -> T,
    mutation: Vec<(usize, &'a str)>,
) -> IsolatedFnCall<T> {
    eprintln!("{pkg_name}:{crate_name}");
    eprintln!("{defs_csv}");

    let mut report = MuttestReportForCrate::from_defs_checked(num_mutables, defs_csv);

    // test if mutable id exists
    for (id, _) in &mutation {
        report.analysis(*id);
    }

    let context = TestContext {
        pkg_name: pkg_name.to_owned(),
        crate_name: crate_name.to_owned(),
        mutations: mutation
            .into_iter()
            .map(|(id, m)| (CrateLocalMutableId { attr_id: 0, id }, Arc::from(m)))
            .collect(),
        details: Mutex::new(BTreeSet::new()),
        coverage: Mutex::new(BTreeMap::new()),
        details_file: Some(Mutex::new(DETAILS_FILE_CSV_HEAD.as_bytes().to_vec())),
        coverage_file: Some(Mutex::new(COVERAGE_FILE_CSV_HEAD.as_bytes().to_vec())),
    };
    TEST_CONTEXT
        .write()
        .unwrap()
        .insert(crate_name.to_owned(), Arc::new(context))
        .ok_or(())
        .expect_err("concurrent execution of test ");

    // perform action
    let res = action();

    // extract data
    let context = TEST_CONTEXT.write().unwrap().remove(crate_name).unwrap();
    eprintln!(
        "{}",
        std::str::from_utf8(&context.details_file.as_ref().unwrap().lock().unwrap()).unwrap()
    );
    eprintln!(
        "{}",
        std::str::from_utf8(&context.coverage_file.as_ref().unwrap().lock().unwrap()).unwrap()
    );

    let context = Arc::try_unwrap(context)
        .ok()
        .expect("someone still has a reference to this context");
    context.extract_data(&mut report);

    IsolatedFnCall { res, report }
}

impl MuttestReportForCrate {
    // TODO: also validate against id collisions?
    pub(crate) fn from_defs_checked(num: usize, defs_csv: &str) -> Self {
        let report = Self::from_definition_csv(defs_csv.as_bytes()).unwrap();
        assert_eq!(num, report.mutables.len(), "expected {num} mutables");
        for id in report.mutables.keys() {
            if num < id.id {
                panic!("invalid id {id}. max: {num}");
            }
        }
        report
    }
}

impl TestContext {
    fn extract_data(self, report: &mut MuttestReportForCrate) {
        report
            .read_details_csv(self.details_file.unwrap().into_inner().unwrap().as_slice())
            .expect("unable to read csv data");
        report
            .read_coverage_csv(self.coverage_file.unwrap().into_inner().unwrap().as_slice())
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
impl<T> ToVec<T> for Vec<T> {
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

impl<R: IMuttestContext> IMuttestContext for Arc<R> {
    fn tracks_mutable(&self, m_id: BakedMutableId) -> bool {
        <R as IMuttestContext>::tracks_mutable(self, m_id)
    }
    fn get_mutation(&self, m_id: CrateLocalMutableId) -> Mutation {
        <R as IMuttestContext>::get_mutation(self, m_id)
    }
    fn write_details(
        &self,
        id: CrateLocalMutableId,
        loc: BakedLocation,
        ty: &str,
        mutations: &str,
    ) {
        <R as IMuttestContext>::write_details(self, id, loc, ty, mutations);
    }
    fn write_coverage(&self, id: CrateLocalMutableId, behavior: Option<&str>) {
        <R as IMuttestContext>::write_coverage(self, id, behavior)
    }
}
impl<R: IMuttestContext + ?Sized> IMuttestContext for Box<R> {
    fn tracks_mutable(&self, m_id: BakedMutableId) -> bool {
        <R as IMuttestContext>::tracks_mutable(self, m_id)
    }
    fn get_mutation(&self, m_id: CrateLocalMutableId) -> Mutation {
        <R as IMuttestContext>::get_mutation(self, m_id)
    }
    fn write_details(
        &self,
        id: CrateLocalMutableId,
        loc: BakedLocation,
        ty: &str,
        mutations: &str,
    ) {
        <R as IMuttestContext>::write_details(self, id, loc, ty, mutations);
    }
    fn write_coverage(&self, id: CrateLocalMutableId, behavior: Option<&str>) {
        <R as IMuttestContext>::write_coverage(self, id, behavior)
    }
}
pub(crate) fn as_box_dyn_context<R: IMuttestContext + 'static>(r: R) -> Box<dyn IMuttestContext> {
    Box::new(r)
}

#[test]
pub fn mutable_id_ord() {
    // TODO: more tests
    assert!(
        BakedMutableId {
            pkg_name: "a",
            crate_name: "b",
            attr_id: 0,
            id: 1,
        } < BakedMutableId {
            pkg_name: "b",
            crate_name: "a",
            attr_id: 0,
            id: 0,
        }
    );
    assert!(
        MutableId {
            crate_id: CrateId {
                pkg_name: "a".to_owned(),
                crate_name: "b".to_owned(),
            },
            id: CrateLocalMutableId { attr_id: 0, id: 1 }
        } < MutableId {
            crate_id: CrateId {
                pkg_name: "b".to_owned(),
                crate_name: "a".to_owned(),
            },
            id: CrateLocalMutableId { attr_id: 0, id: 1 }
        }
    );
}
