// TODO: fix lifetime extension issues
// #[muttest_codegen::mutate_isolated("lit_str")]
// fn promoted_array() -> impl Iterator<Item = String> {
//     ["true"]
//         .iter()
//         .map(|&s| s.into())
// }

// #[muttest_codegen::mutate_isolated("lit_int")]
// fn promoted_int() -> &'static u8 {
//     &1
// }
