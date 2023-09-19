use std::{fmt, process::ExitCode};

use quote::{format_ident, quote_spanned};
use syn::{fold::Fold, parse_quote_spanned};

use super::*;

pub struct MuttestTestTransformer {
    pub conf: TransformerConf,
    pub test_count: u32,
}

impl Fold for MuttestTestTransformer {
    fn fold_item_fn(&mut self, f: syn::ItemFn) -> syn::ItemFn {
        let mut f = syn::fold::fold_item_fn(self, f);

        if has_attr(&f.attrs, "test") {
            let should_panic = has_attr(&f.attrs, "should_panic");
            self.test_count += 1;
            let id = self.test_count;
            let attr_id = self.conf.attr_id;

            let test_name = f.sig.ident.to_string();
            let muttest_api = &self.conf.muttest_api;
            let pkg_name = &self.conf.crate_id.pkg_name;
            let crate_name = &self.conf.crate_id.crate_name;

            let test_skip = if should_panic {
                quote_spanned!(self.conf.span=> panic!())
            } else {
                quote_spanned!(self.conf.span=> return #muttest_api::TestSkip::test_skip())
            };
            // TODO: dont skip test if `TestSkip` not supported
            f.sig.ident =
                format_ident!("{}{}", TEST_PREFIX, f.sig.ident, span = f.sig.ident.span());
            f.block.stmts.insert(
                0,
                parse_quote_spanned! {self.conf.span=>
                    let #muttest_api::ControlFlow::Continue(_test_token) =
                        #muttest_api::MuttestTest {
                            id: #muttest_api::TestId {
                                crate_id: #muttest_api::CrateId {
                                    pkg_name: #muttest_api::Cow::Borrowed(#pkg_name),
                                    crate_name: #muttest_api::Cow::Borrowed(#crate_name),
                                },
                                id: #muttest_api::CrateLocalTestId {
                                    attr_id: #attr_id,
                                    id: #id,
                                }
                            },
                            name: #test_name,
                            module: #muttest_api::module_path!(),
                            file: #muttest_api::file!(),
                            should_panic: #should_panic,
                            running: #muttest_api::Option::None,
                        }.start() else { #test_skip };
                },
            );
        }

        f
    }
}

pub trait TestSkip {
    fn test_skip() -> Self;
}
impl TestSkip for () {
    fn test_skip() -> Self {}
}
impl<T: TestSkip, E: fmt::Debug> TestSkip for Result<T, E> {
    fn test_skip() -> Self {
        Ok(T::test_skip())
    }
}
impl TestSkip for ExitCode {
    fn test_skip() -> Self {
        ExitCode::SUCCESS
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_skip_unit() {
        TestSkip::test_skip()
    }

    #[test]
    fn test_skip_io_result() -> std::io::Result<()> {
        TestSkip::test_skip()
    }

    #[test]
    fn test_skip_anyhow_result() -> anyhow::Result<()> {
        TestSkip::test_skip()
    }

    #[test]
    fn test_skip_exit_code() -> ExitCode {
        TestSkip::test_skip()
    }
}
