use std::{
    cell::RefCell,
    collections::{BTreeMap, BTreeSet},
    ops::Deref,
    sync::{Arc, Mutex, Once, RwLock},
};

use log::{info, trace};

use crate::{
    context::{IMuttestContext, MuttestContext, COVERAGE_FILE_CSV_HEAD, DETAILS_FILE_CSV_HEAD},
    mutable_id::CrateId,
    report::{MutableReport, MuttestReportForCrate},
    BakedLocation, BakedMutableId, CrateLocalMutableId, MutableId, Mutation,
};

pub use crate::{call_isolated, data_isolated};

// TODO: BTreeMap::new is not yet const :/

type TestContext = MuttestContext<Vec<u8>>;
lazy_static::lazy_static! {
    static ref TEST_CONTEXT: RwLock<BTreeMap<String, Arc<TestContext>>> =
        RwLock::new(BTreeMap::new());
}

thread_local! {
    /// describes the nesting in the testcase for selftest
    static SELFTEST_NESTING: RefCell<SelftestNesting> = RefCell::new(SelftestNesting::default());
}

#[derive(Default)]
struct SelftestNesting {
    current: Option<CrateLocalMutableId>,
    nested: Option<CrateLocalMutableId>,
}

pub(crate) fn selftest_mutation_nested(m_id: CrateLocalMutableId) -> bool {
    assert_ne!(m_id.attr_id, 0, "attr_id: 0 is for isolated mutations");

    SELFTEST_NESTING.with(move |sn| {
        let mut sn = sn.borrow_mut();
        if sn.current.is_none() {
            trace!("OPEN {}", m_id);
            assert_eq!(sn.nested, None);
            sn.current = Some(m_id);
            false
        } else {
            trace!("    nested {} in {}", m_id, sn.current.unwrap(),);
            assert_eq!(
                sn.nested,
                None,
                "invalid skip handling {m_id} in {}->{}",
                sn.current.unwrap(),
                sn.nested.unwrap(),
            );
            sn.nested = Some(m_id);
            true
        }
    })
}
impl Drop for Mutation {
    fn drop(&mut self) {
        // only perform nesting-handling for selftest-mutations
        let Some(source) = self.source else {return};
        if source.attr_id == 0 {
            return;
        }

        SELFTEST_NESTING.with(|sn| {
            let mut sn = sn.borrow_mut();
            if self.skip {
                trace!("    un-nest {}", source);
                assert_eq!(sn.nested, Some(source), "invalid nesting");
                sn.nested = None;
            } else {
                trace!("CLOSE {}", source);
                assert_eq!(sn.nested, None);
                assert_eq!(sn.current, Some(source), "invalid nesting");
                sn.current = None;
            }
        });
    }
}
// #[macro_export]
// macro_rules! return_early_if_nesting {
//     ($m_id:expr, $e:expr) => {
//         let __muttest_nesting_token = {
//             let mut t = $crate::tests::NestingToken::create($m_id);
//             if t.is_nested() {
//                 return $e;
//             }
//             t
//         };
//     };
// }

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
    pub(crate) fn for_mutable(&self, id: usize) -> &MutableReport {
        self.mutables
            .iter()
            .find(|(m_id, _)| m_id.id == id)
            .unwrap_or_else(|| panic!("no mutable wih {id} found"))
            .1
    }
}

#[macro_export]
macro_rules! call_isolated {
    ($f:ident $(::<$t:ty>)? ($($args:expr),*) $(where $m_id:expr => $m:expr)?) => {{
        $crate::tests::test_setup_once();
        $crate::tests::run$(::<$t>)?(
            $f::PKG_NAME,
            $f::CRATE_NAME,
            $f::NUM_MUTABLES,
            $f::MUTABLES_CSV,
            || $f($($args),*),
            vec![$(($m_id, $m))?]
        )
    }};
}
#[macro_export]
macro_rules! data_isolated {
    ($f:ident) => {{
        $crate::tests::test_setup_once();
        ::log::info!("{}:{}", $f::PKG_NAME, $f::CRATE_NAME);
        ::log::info!("{}", $f::MUTABLES_CSV);
        $crate::report::MuttestReportForCrate::from_defs_checked($f::NUM_MUTABLES, $f::MUTABLES_CSV)
    }};
}

pub(crate) fn test_setup_once() {
    static TEST_SETUP: Once = Once::new();
    TEST_SETUP.call_once(|| {
        unsafe {
            backtrace_on_stack_overflow::enable();
        };
        env_logger::init();
    });
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
    info!("{pkg_name}:{crate_name}");
    info!("{defs_csv}");

    let mut report = MuttestReportForCrate::from_defs_checked(num_mutables, defs_csv);

    // test if mutable id exists in report
    for (id, _) in &mutation {
        report.for_mutable(*id);
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
    let mut context =
        Arc::try_unwrap(context).expect("someone still has a reference to this context");

    eprintln!(
        "{}",
        std::str::from_utf8(context.details_file.as_mut().unwrap().get_mut().unwrap()).unwrap()
    );
    eprintln!(
        "{}",
        std::str::from_utf8(context.coverage_file.as_mut().unwrap().get_mut().unwrap()).unwrap()
    );

    context.extract_data(&mut report);

    IsolatedFnCall { res, report }
}

impl MuttestReportForCrate {
    pub(crate) fn from_defs_checked(num: usize, defs_csv: &str) -> Self {
        let report = Self::from_definition_csv(defs_csv.as_bytes()).unwrap();
        assert_eq!(num, report.mutables.len(), "expected {num} mutables");
        for id in report.mutables.keys() {
            if num < id.id {
                panic!("invalid id {id}. max: {num}");
            }
            assert_eq!(id.attr_id, 0);
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

pub trait ToVecExt<T> {
    fn to_vec_cloned(&self) -> Vec<T>
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
impl<T> ToVecExt<T> for BTreeSet<T> {
    fn to_vec_cloned(&self) -> Vec<T>
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
impl<T> ToVecExt<T> for Vec<T> {
    fn to_vec_cloned(&self) -> Vec<T>
    where
        T: Clone,
    {
        <[T]>::to_vec(self)
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
impl<T> ToVecExt<T> for &[T] {
    fn to_vec_cloned(&self) -> Vec<T>
    where
        T: Clone,
    {
        <[T]>::to_vec(self)
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
impl<T, const N: usize> ToVecExt<T> for [T; N] {
    fn to_vec_cloned(&self) -> Vec<T>
    where
        T: Clone,
    {
        <[T]>::to_vec(self)
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
