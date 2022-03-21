use std::sync::atomic::{AtomicUsize, Ordering};

pub use muttest_codegen::mutate;

use lazy_static::lazy_static;

lazy_static! {
    static ref ACTIVE_MUTATION: AtomicUsize = {
        // TODO: report errors
            AtomicUsize::new(std::env::var("MUTTEST_MUTATION")
                .ok()
                .and_then(|v| v.parse().ok()).unwrap_or_default())
    };
}

// TODO: feature-gate export of these functions
pub fn set_mutation(m_id: usize) {
    ACTIVE_MUTATION.store(m_id, Ordering::SeqCst);
}
pub fn get_active_mutation() -> usize {
    ACTIVE_MUTATION.load(Ordering::SeqCst)
}
pub fn get_mutation_for(m_id: usize) -> usize {
    ACTIVE_MUTATION.load(Ordering::SeqCst).wrapping_sub(m_id)
}
