use std::{
    fs,
    io::Write,
    ops::Sub,
    path::PathBuf,
    sync::{
        atomic::{AtomicUsize, Ordering},
        Mutex,
    },
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
    // TODO: report errors
    static ref ACTIVE_MUTATION: AtomicUsize = {
        AtomicUsize::new(std::env::var("MUTTEST_MUTATION")
            .ok()
            .and_then(|v| v.parse().ok()).unwrap_or_default())
    };
}

fn save_msg(msg: &str) {
    let mut f = LOGGER.lock().unwrap();
    writeln!(&mut f, "{}", msg).expect("unable to write log");
    f.flush().expect("unable to flush log");
}

// TODO: feature-gate export of this function
pub fn set_mutation(m_id: usize) {
    ACTIVE_MUTATION.store(m_id, Ordering::SeqCst);
}
pub fn get_active_mutation() -> usize {
    ACTIVE_MUTATION.load(Ordering::SeqCst)
}
/// get the active mutation for a mutable
pub fn get_active_mutation_for_mutable(m_id: usize) -> usize {
    ACTIVE_MUTATION
        .load(Ordering::SeqCst)
        .saturating_sub(m_id - 1)
}
pub fn mutable_behavior() -> Option<usize> {
    None
}
fn mutable_covered(m_id: usize, module: &str) {
    save_msg(&format!("COVERED {m_id} in {module}"));
}

pub fn mutable_int<T: MutableInt>(m_id: usize, module: &str, x: T) -> T {
    mutable_covered(m_id, module);
    match get_active_mutation_for_mutable(m_id) {
        1 => x.increment(),
        _ => x,
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
    save_msg(&format!("CMP {m_id:?}; {ord:?}"));
    if let Some(ord) = ord {
        match get_active_mutation_for_mutable(m_id) {
            1 => match op_str {
                "<" => ord.reverse().is_lt(),
                "<=" => ord.reverse().is_le(),
                ">=" => ord.reverse().is_ge(),
                ">" => ord.reverse().is_gt(),
                _ => unreachable!(),
            },
            _ => match op_str {
                "<" => ord.is_lt(),
                "<=" => ord.is_le(),
                ">=" => ord.is_ge(),
                ">" => ord.is_gt(),
                _ => unreachable!(),
            },
        }
    } else {
        false
    }
}

pub fn mutable_bin_op(m_id: usize, module: &'static str, op_str: &'static str) -> &'static str {
    mutable_covered(m_id, module);
    match get_active_mutation_for_mutable(m_id) {
        1 => "-",
        _ => op_str,
    }
}

// pub trait MutableBinOpAdd: Sized {}

pub trait MutableInt: Copy {
    fn increment(self) -> Self;
}
macro_rules! mutable_ints {
    ($($t:ty),*) => {
        $(impl MutableInt for $t {
            fn increment(self) -> Self {
                self + 1
            }
        })*
    };
}
mutable_ints!(u8, u16, u32, u64, u128, usize, i8, i16, i32, i64, i128, isize);

pub trait IsYesSub {
    fn get_sub(&self) -> YesSub;
}
pub trait IsNotSub {
    fn get_sub(&self) -> NotSub;
}
impl<T: Sub<T, Output = T>> IsYesSub for T {
    fn get_sub(&self) -> YesSub {
        YesSub
    }
}
impl<T> IsNotSub for &T {
    fn get_sub(&self) -> NotSub {
        NotSub
    }
}
pub struct YesSub;
pub struct NotSub;

impl YesSub {
    pub fn is_sub(&self) -> bool {
        true
    }
    pub fn sub<T: Sub<T, Output = T>>(self, left: T, right: T) -> T {
        left.sub(right)
    }
}
impl NotSub {
    pub fn is_sub(&self) -> bool {
        false
    }
    pub fn sub<T>(self, _: T, _: T) -> T {
        panic!()
    }
}
