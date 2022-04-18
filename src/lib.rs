use std::{
    collections::BTreeMap,
    fs,
    io::Write,
    marker::PhantomData,
    path::PathBuf,
    sync::{Mutex, RwLock},
};

pub use muttest_codegen::mutate;

use lazy_static::lazy_static;

lazy_static! {
    static ref MUTTEST_DIR: PathBuf = {
        let mut directory = PathBuf::from(
            std::env::var("CARGO_MANIFEST_DIR").expect("unable to get cargo manifest dir"),
        );
        directory.push("target/muttest");
        directory
    };
    static ref LOGGER: Mutex<fs::File> = {
        fs::create_dir_all(&*MUTTEST_DIR).expect("unable to create muttest directory");
        let filename = "cover.log";
        let file = fs::File::create(MUTTEST_DIR.join(filename)).expect("unable to open logger file");
        Mutex::new(file)
        // TODO: log to different files for integration test (set file in runner)
    };
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
        let m = m.split("=").collect::<Vec<_>>();
        mutations.insert(m[0].parse().unwrap(), m[1].to_owned());
    }

    mutations
}

fn save_msg(msg: &str) {
    let mut f = LOGGER.lock().unwrap();
    writeln!(&mut f, "{}", msg).expect("unable to write log");
    f.flush().expect("unable to flush log");
}

// TODO: feature-gate export of this function
pub fn set_mutation(m_id: usize, mutation: String) {
    ACTIVE_MUTATION
        .write()
        .expect("write-lock active mutations")
        .insert(m_id, mutation);
}
/// get the active mutation for a mutable
pub fn get_active_mutation_for_mutable(m_id: usize) -> Option<String> {
    // TODO: without copy via raii
    ACTIVE_MUTATION
        .read()
        .expect("read-lock active mutations")
        .get(&m_id)
        .cloned()
}
pub fn mutable_behavior() -> Option<usize> {
    None
}
fn mutable_covered(m_id: usize, module: &str) {
    save_msg(&format!("COVERED {m_id} in {module}"));
}

pub fn mutable_int<T: MutableInt>(m_id: usize, module: &str, x: T) -> T {
    mutable_covered(m_id, module);
    match get_active_mutation_for_mutable(m_id).as_deref() {
        None => x,
        Some(p) if p.chars().all(|c| c.is_numeric()) => T::parse(p),
        Some("+1") => x.increment(),
        _ => todo!(),
    }
}

pub fn mutable_cmp<T: PartialOrd<T1>, T1>(
    m_id: usize,
    module: &str,
    op_str: &str,
    left: &T,
    right: &T1,
) -> bool {
    mutable_covered(m_id, module);
    let ord = left.partial_cmp(right);
    save_msg(&format!("CMP {m_id}; {ord:?}"));
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

pub fn mutable_bin_op(m_id: usize, module: &'static str, _op_str: &'static str) -> &'static str {
    mutable_covered(m_id, module);
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
pub fn report_possible_mutations(m_id: usize, reports: &[(&str, bool)]) {
    let mutations = reports
        .iter()
        .filter(|(_, ok)| *ok)
        .map(|(m, _)| *m)
        .collect::<Vec<_>>();
    save_msg(&format!("CALC {m_id}: {mutations:?}"));
}

pub fn phantom_for_type<T>(_: &T) -> PhantomData<T> {
    PhantomData
}

pub trait MutableInt: Copy {
    fn parse(s: &str) -> Self;
    fn increment(self) -> Self;
}
macro_rules! mutable_ints {
    ($($t:ty),*) => {
        $(impl MutableInt for $t {
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
