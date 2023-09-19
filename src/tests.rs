use std::{
    collections::{BTreeMap, BTreeSet},
    ops::Deref,
    sync::{Mutex, Once, RwLock},
};

use crate::{
    context::{
        AnalysisContext, MuttestContext, Writer, COVERAGE_FILE_CSV_HEAD, TYPES_FILE_CSV_HEAD,
    },
    mutator::MutatorFns,
    report::{
        CrateId, CrateLocalTestId, MutableAnalysis, MutableReport, MuttestReport,
        MuttestReportForCrate, TestId,
    },
    CrateLocalMutableId, MutableId,
};

pub use crate::{call_isolated, data_isolated};

static ISOLATED_TEST_CONTEXTS: RwLock<BTreeMap<CrateId, &'static MuttestContext>> =
    RwLock::new(BTreeMap::new());

impl MutableId {
    pub(crate) fn is_isolated(&self) -> bool {
        self.crate_id.pkg_name == "#isolated"
    }

    pub(crate) fn test_context(&self) -> Option<&'static MuttestContext> {
        if self.is_isolated() {
            Some(
                ISOLATED_TEST_CONTEXTS
                    .read()
                    .unwrap()
                    .get(&self.crate_id)
                    .expect("context for this testcase not set"),
            )
        } else {
            super::MUTTEST_CONTEXT.as_ref()
        }
    }
}

impl MuttestReportForCrate {
    pub(crate) fn for_mutable(&self, id: u32) -> &MutableReport {
        self.mutables
            .iter()
            .find(|(m_id, _)| m_id.id == id)
            .unwrap_or_else(|| panic!("no mutable wih {id} found"))
            .1
    }
}
impl MutableAnalysis {
    pub(crate) fn behavior(&self) -> Vec<&str> {
        self.behavior
            .values()
            .next()
            .unwrap()
            .iter()
            .map(Deref::deref)
            .collect()
    }
}

#[macro_export]
macro_rules! call_isolated {
    ($f:ident $(::<$t:ty>)? ($($args:expr),*) $(where $m_id:literal: $m:expr)?) => {{
        $crate::tests::run$(::<$t>)?(
            $f::CRATE_ID,
            $f::NUM_MUTABLES,
            $f::MUTABLES_CSV,
            || $f($($args),*),
            vec![$(($m_id, $m))?].into_iter().next()
        )
    }};
    ($module:ident: $e:expr $(;where $m_id:literal: $m:expr)?) => {{
        $crate::tests::run(
            $module::CRATE_ID,
            $module::NUM_MUTABLES,
            $module::MUTABLES_CSV,
            || $e,
            vec![$(($m_id, $m))?].into_iter().next()
        )
    }};
}

#[macro_export]
macro_rules! data_isolated {
    ($f:ident) => {{
        $crate::tests::test_setup_once();
        println!("{}", $f::CRATE_ID);
        println!("{}", $f::MUTABLES_CSV);
        $crate::report::MuttestReportForCrate::from_defs_checked($f::NUM_MUTABLES, $f::MUTABLES_CSV)
    }};
}

pub(crate) fn test_setup_once() {}

pub struct IsolatedFnCall<T> {
    pub res: T,
    pub report: MuttestReportForCrate,
}

pub fn run<T>(
    crate_id: CrateId,
    num_mutables: u32,
    defs_csv: &str,
    action: impl FnOnce() -> T,
    mutation: Option<(u32, &'static str)>,
) -> IsolatedFnCall<T> {
    static TEST_SETUP: Once = Once::new();
    TEST_SETUP.call_once(|| {
        unsafe {
            backtrace_on_stack_overflow::enable();
        };
    });

    println!("{crate_id}");
    println!("{defs_csv}");

    let mut report = MuttestReport::default();
    report.muttest_crates.insert(
        crate_id.clone(),
        MuttestReportForCrate::from_defs_checked(num_mutables, defs_csv),
    );

    // test if mutable id exists in report
    for (id, _) in &mutation {
        report.muttest_crates[&crate_id].for_mutable(*id);
    }

    let context = MuttestContext {
        mutation: mutation.map(|(id, m)| {
            (
                MutableId {
                    crate_id: crate_id.clone(),
                    id: CrateLocalMutableId { attr_id: 0, id },
                },
                m,
            )
        }),
        relevant_tests: None,
        test_deadline: None,
        test_default_timeout: Default::default(),
        test_mutex: None,
        analysis: Some(AnalysisContext {
            types_map: Mutex::new(BTreeSet::new()),
            coverage_map: Mutex::new(BTreeMap::new()),
            types_file: Writer::InMemory(Mutex::new(TYPES_FILE_CSV_HEAD.to_owned())),
            coverage_file: Writer::InMemory(Mutex::new(COVERAGE_FILE_CSV_HEAD.to_owned())),
            location_file: Writer::InMemory(Mutex::new(Default::default())),
            attr_location_file: Writer::InMemory(Mutex::new(Default::default())),
            // location files are not used in tests
        }),
        test_results_file: None,
    };
    let context = Box::leak(Box::new(context));
    ISOLATED_TEST_CONTEXTS
        .write()
        .unwrap()
        .insert(crate_id.clone(), context)
        .ok_or(())
        .expect_err("concurrent execution of test ");

    // perform action
    let res = action();

    // write
    context.end_test(
        &TestId {
            crate_id: crate_id.clone(),
            id: CrateLocalTestId { attr_id: 0, id: 0 },
        },
        true,
    );

    // extract data
    let context = ISOLATED_TEST_CONTEXTS
        .write()
        .unwrap()
        .remove(&crate_id)
        .unwrap();

    println!(
        "analysis\n{}",
        context.analysis.as_ref().unwrap().types_file.view().deref()
    );
    println!(
        "coverage\n{}",
        context
            .analysis
            .as_ref()
            .unwrap()
            .coverage_file
            .view()
            .deref()
    );

    context.extract_data(&mut report);

    let report = report.muttest_crates.into_values().next().unwrap();

    IsolatedFnCall { res, report }
}

impl Writer {
    fn view(&self) -> impl Deref<Target = String> + '_ {
        match self {
            Writer::InMemory(s) => s.try_lock().unwrap(),
            _ => panic!("not InMemory"),
        }
    }
}

impl MuttestReportForCrate {
    pub(crate) fn from_defs_checked(num: u32, defs_csv: &str) -> Self {
        let report = Self::from_definition_csv(defs_csv.as_bytes()).unwrap();
        assert_eq!(num, report.mutables.len() as u32, "expected {num} mutables");
        for id in report.mutables.keys() {
            if num < id.id {
                panic!("invalid id {id}. max: {num}");
            }
            assert_eq!(id.attr_id, 0);
        }
        report
    }
}

impl MuttestContext {
    fn extract_data(&self, report: &mut MuttestReport) {
        report
            .read_types_csv(self.analysis.as_ref().unwrap().types_file.view().as_bytes())
            .expect("unable to read csv data");
        report
            .read_coverage_csv(
                self.analysis
                    .as_ref()
                    .unwrap()
                    .coverage_file
                    .view()
                    .as_bytes(),
            )
            .expect("unable to read csv data");
    }
}

pub(crate) fn eval_identical_behavior_mutations(
    mutator: &dyn MutatorFns,
    analysis: &MutableAnalysis,
) -> (BTreeSet<String>, BTreeSet<String>) {
    mutator
        .valid_mutations(analysis)
        .into_iter()
        .partition(|m| {
            mutator.identical_behavior(
                &analysis.code,
                analysis.behavior.values().next().unwrap(),
                m,
            )
        })
}

pub trait VecExt {
    fn sorted(self) -> Self;
}
impl<T: Ord> VecExt for Vec<T> {
    fn sorted(mut self) -> Self {
        self.sort();
        self
    }
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
        MutableId {
            crate_id: CrateId {
                pkg_name: "a".to_owned().into(),
                crate_name: "b".to_owned().into(),
            },
            id: CrateLocalMutableId { attr_id: 0, id: 1 }
        } < MutableId {
            crate_id: CrateId {
                pkg_name: "b".to_owned().into(),
                crate_name: "a".to_owned().into(),
            },
            id: CrateLocalMutableId { attr_id: 0, id: 0 }
        }
    );
    assert!(
        MutableId {
            crate_id: CrateId {
                pkg_name: "a".to_owned().into(),
                crate_name: "b".to_owned().into(),
            },
            id: CrateLocalMutableId { attr_id: 0, id: 1 }
        } < MutableId {
            crate_id: CrateId {
                pkg_name: "b".to_owned().into(),
                crate_name: "a".to_owned().into(),
            },
            id: CrateLocalMutableId { attr_id: 0, id: 1 }
        }
    );
}
