use std::{
    collections::{BTreeMap, BTreeSet},
    env::VarError,
    fmt::{self, Display},
    fs,
    io::Write,
    marker::PhantomData,
    ops::DerefMut,
    path::PathBuf,
    sync::{Mutex, RwLock},
};

pub use muttest_codegen::mutate;

use lazy_static::lazy_static;

// TODO: make stubbable
lazy_static! {
    static ref MUTTEST_DIR: PathBuf = {
        // TODO: read some env var
        let dir = PathBuf::from("target/muttest");
        fs::create_dir_all(&dir).expect("unable to create muttest directory");
        dir
    };
    static ref COVERAGE_FILE: Mutex<fs::File> = {
        let filename = "cover.log";
        let file = fs::File::create(MUTTEST_DIR.join(filename)).expect("unable to open logger file");
        Mutex::new(file)
        // TODO: log to different files for integration test (set file in runner)
    };
    static ref MUTABLE_DETAILS_FILE: Mutex<Option<fs::File>> = {
        Mutex::new(open_details_file().expect("unable to open details file"))
    };
    static ref MUTABLE_DETAILS: Mutex<BTreeSet<(MutableId, &'static str)>> = Default::default();
    static ref ACTIVE_MUTATION: RwLock<BTreeMap<usize, String>> = {
        RwLock::new(parse_active_mutations(&std::env::var("MUTTEST_MUTATION").unwrap_or_default()))
    };
}
fn parse_active_mutations(env: &str) -> BTreeMap<usize, String> {
    let mut mutations = BTreeMap::new();

    // TODO: report errors
    for m in env.split(";") {
        if m.is_empty() {
            continue;
        }
        let m = m.split_once(":").unwrap();
        mutations.insert(m.0.parse().unwrap(), m.1.to_owned());
    }

    mutations
}
// TODO: use MuttestError instead
fn open_details_file() -> Result<Option<fs::File>, std::io::Error> {
    match std::env::var("MUTTEST_DETAILS_FILE") {
        Err(VarError::NotPresent) => Ok(None),
        Err(_) => todo!("not unicode error"),
        Ok(f) => Ok(Some(
            fs::OpenOptions::new()
                .read(true)
                .write(true)
                .append(true)
                .open(f)?,
        )),
    }
    // TODO: read already reported messages
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MutableId {
    pub id: usize,
    pub crate_name: &'static str,
}
impl fmt::Display for MutableId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.id, self.crate_name)
    }
}

fn report_coverage(m_id: MutableId) {
    let mut f = COVERAGE_FILE.lock().unwrap();
    writeln!(&mut f, "{m_id}").expect("unable to write log");
    f.flush().expect("unable to flush coverage report");
}

fn report_detail<T: Display>(m_id: MutableId, kind: &'static str, data: T) {
    let is_new = MUTABLE_DETAILS.lock().unwrap().insert((m_id, kind));
    if !is_new {
        return;
    }
    let mut f = MUTABLE_DETAILS_FILE.lock().unwrap();
    if let Some(f) = f.deref_mut() {
        writeln!(f, "{m_id},{kind},{data}").expect("unable to write mutable context");
        f.flush()
            .expect("unable to flush possible mutations report");
    }
}

pub fn report_possible_mutations(m_id: MutableId, reports: &[(&str, bool)]) {
    let mutations = reports
        .iter()
        .filter(|(_, ok)| *ok)
        .map(|(m, _)| *m)
        .collect::<Vec<_>>();
    let mutations = mutations.join(":");
    report_detail(m_id, "mutations", mutations);
}

pub fn report_mutable_type(m_id: MutableId, ty: &str) {
    report_detail(m_id, "type", ty);
}

pub fn report_location(m_id: MutableId, file: &'static str, line: u32, column: u32) {
    report_detail(m_id, "loc", MutableLocation { file, line, column })
}

/// get the active mutation for a mutable
pub fn get_active_mutation_for_mutable(m_id: MutableId) -> Option<String> {
    // TODO: somehow consider m_id.crate_name
    ACTIVE_MUTATION
        .read()
        .expect("read-lock active mutations")
        .get(&m_id.id)
        .cloned()
}

#[derive(Debug, Clone, Copy)]
#[allow(unused)]
pub struct MutableLocation {
    file: &'static str,
    line: u32,
    column: u32,
}
impl fmt::Display for MutableLocation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}:{}", self.file, self.line, self.column)
    }
}

pub fn mutable_int<T: MutableInt>(m_id: MutableId, x: T) -> T {
    report_coverage(m_id);
    report_mutable_type(m_id, T::type_str());
    match get_active_mutation_for_mutable(m_id).as_deref() {
        None => x,
        Some(p) if p.chars().all(|c| c.is_numeric()) => T::parse(p),
        Some("+1") => x.increment(),
        _ => todo!(),
    }
}

pub fn mutable_cmp<T: PartialOrd<T1>, T1>(
    m_id: MutableId,
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

pub fn mutable_bin_op(m_id: MutableId, _op_str: &'static str) -> &'static str {
    report_coverage(m_id);
    match get_active_mutation_for_mutable(m_id).as_deref() {
        None => "",
        Some("-") => "-",
        Some("+") => "+",
        Some("*") => "*",
        Some("/") => "/",
        Some("%") => "%",
        _ => todo!(),
    }
}
pub fn phantom_for_type<T>(_: &T) -> PhantomData<T> {
    PhantomData
}

pub trait MutableInt: Copy + fmt::Display {
    fn type_str() -> &'static str;
    fn parse(s: &str) -> Self;
    fn increment(self) -> Self;
}
macro_rules! mutable_ints {
    ($($t:ty),*) => {
        $(impl MutableInt for $t {
            fn type_str() -> &'static str {
                stringify!($t)
            }
            fn increment(self) -> Self {
                self + 1
            }
            fn parse(s: &str) -> Self {
                s.parse().expect("unable to parse number")
            }
        })*
    };
}
mutable_ints!(u8, u16, u32, u64, u128, usize, i8, i16, i32, i64, i128, isize);

macro_rules! binop_mutation {
    ($m:ident, $t:path, $f:ident) => {
        pub mod $m {
            use core::marker::PhantomData;

            pub struct Yes;
            pub struct No;

            pub trait IsYes {
                fn get_impl(&self) -> Yes;
            }
            pub trait IsNo {
                fn get_impl(&self) -> No;
            }
            impl<L: $t, R> IsYes
                for (
                    PhantomData<L>,
                    PhantomData<R>,
                    PhantomData<<L as $t>::Output>,
                )
            {
                fn get_impl(&self) -> Yes {
                    Yes
                }
            }
            impl<L, R, O> IsNo for &(PhantomData<L>, PhantomData<R>, PhantomData<O>) {
                fn get_impl(&self) -> No {
                    No
                }
            }
            impl Yes {
                pub fn is_impl(&self) -> bool {
                    true
                }
                pub fn run<L: $t, R>(self, left: L, right: R) -> <L as $t>::Output {
                    <L as $t>::$f(left, right)
                }
            }
            impl No {
                pub fn is_impl(&self) -> bool {
                    false
                }
                pub fn run<L, R, O>(self, _: L, _: R) -> O {
                    panic!()
                }
            }
        }
    };
}

binop_mutation!(add, std::ops::Add<R>, add);
binop_mutation!(sub, std::ops::Sub<R>, sub);
binop_mutation!(mul, std::ops::Mul<R>, mul);
binop_mutation!(div, std::ops::Div<R>, div);
binop_mutation!(rem, std::ops::Rem<R>, rem);

#[macro_export]
macro_rules! get_binop {
    ($op:ident, $left:expr, $right:expr, $o:expr) => {{
        #[allow(unused_imports)]
        use ::muttest::$op::{IsNo, IsYes};
        (&($left, $right, $o)).get_impl()
    }};
}
