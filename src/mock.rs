use std::{borrow::Cow, collections::BTreeMap, sync::Mutex};

use lazy_static::lazy_static;

use crate::MutableId;

lazy_static! {
    static ref MOCK_LOCK: Mutex<()> = Mutex::new(());
}
pub fn without_mutation<T>(action: impl Fn() -> T) -> T {
    run_internal(BTreeMap::new(), action)
}
pub fn with_mutation<T>(id: usize, mutation: &str, action: impl Fn() -> T) -> T {
    let mut m = BTreeMap::new();
    m.insert(
        MutableId {
            id,
            crate_name: Cow::Borrowed(""),
        },
        mutation.to_owned(),
    );
    run_internal(m, action)
}
fn run_internal<T>(mutation: BTreeMap<MutableId<'static>, String>, action: impl Fn() -> T) -> T {
    let l = MOCK_LOCK.lock();
    *crate::ACTIVE_MUTATION.write().unwrap() = mutation;
    let res = action();
    std::mem::drop(l);
    res
}
