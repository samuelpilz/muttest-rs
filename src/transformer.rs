use std::path::PathBuf;

use proc_macro2::{Span, TokenStream};
use quote::quote_spanned;

use crate::{
    display_or_empty_if_none, mutable::Mutable, mutable_id::CrateId, CrateLocalMutableId,
    MutableId, PathSegment,
};

pub const MUTABLE_DEFINITIONS_CSV_HEAD: &str = "attr_id,id,kind,code,file,path,attr_span,span\n";

pub struct MuttestTransformer {
    pub conf: TransformerConf,
    mut_count: usize,
    definitions: Vec<String>,
    pub path: Vec<PathSegment>,
}
pub struct TransformerConf {
    pub attr_id: usize,
    pub span: Span,
    pub mutables: MutablesConf,
    pub muttest_api: TokenStream,
    pub pkg_name: &'static str,
    pub crate_name: String,
}
pub enum MutablesConf {
    All,
    One(String),
}

impl MuttestTransformer {
    pub fn new(conf: TransformerConf) -> Self {
        Self {
            conf,
            mut_count: 0,
            definitions: vec![],
            path: vec![],
        }
    }

    pub fn definitions(&self) -> &[String] {
        &self.definitions
    }
    pub fn num_mutables(&self) -> usize {
        self.mut_count
    }

    pub fn should_mutate(&self, mutable: &str) -> bool {
        match &self.conf.mutables {
            MutablesConf::One(m) => m == mutable,
            MutablesConf::All => true,
        }
    }

    pub(crate) fn new_mutable<'b, M: Mutable<'b>>(
        &mut self,
        m: &M,
        code: &str,
    ) -> TransformSnippets {
        self.mut_count += 1;
        let next_id = self.mut_count;
        let id = MutableId {
            crate_id: CrateId {
                pkg_name: self.conf.pkg_name.to_owned(),
                crate_name: self.conf.crate_name.clone(),
            },
            id: CrateLocalMutableId {
                attr_id: self.conf.attr_id,
                id: next_id,
            },
        };

        let span = m.span();
        // TODO: use csv crate instead.
        self.definitions.push(format!(
            r#"{},{},{},{},{},{},{},{}"#,
            id.id.attr_id,
            id.id.id,
            M::NAME,
            csv_quote(code),
            source_file_path(span).unwrap_or_default().display(),
            self.path
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join(":"),
            display_or_empty_if_none(&crate::Span::from(self.conf.span)),
            display_or_empty_if_none(&crate::Span::from(span)),
        ));

        let muttest_api = self.muttest_api(span);
        let pkg_name = &id.crate_id.pkg_name;
        let crate_name = &id.crate_id.crate_name;
        let attr_id = id.id.attr_id;
        let id = id.id.id;
        let m_id = quote_spanned! {span=>
            #muttest_api::BakedMutableId {
                pkg_name: #pkg_name,
                crate_name: #crate_name,
                attr_id: #attr_id,
                id: #id,
            }
        };
        let attr_span = self.bake_span(self.conf.span);
        let mutable_span = self.bake_span(span);

        let loc = quote_spanned! {span=>
            #muttest_api::BakedLocation {
                file: #muttest_api::file!(),
                module: #muttest_api::module_path!(),
                attr_span: #attr_span,
                span: #mutable_span,
            }
        };

        TransformSnippets {
            m_id,
            muttest_api,
            loc,
        }
    }

    fn bake_span(&self, span: Span) -> TokenStream {
        // TODO: use Span::before/after when available to get the entire range
        let muttest_api = self.muttest_api(span);
        quote_spanned! {span=>
            #muttest_api::Span {
                start: #muttest_api::LineColumn {line: #muttest_api::line!(), column: #muttest_api::column!() },
                end: #muttest_api::Option::None,
            }
        }
    }

    pub fn muttest_api(&self, span: Span) -> TokenStream {
        fn relocate_token_stream(input: TokenStream, span: Span) -> TokenStream {
            use proc_macro2::{Group, TokenTree};
            input
                .into_iter()
                .map(|mut tt| {
                    match &mut tt {
                        TokenTree::Group(g) => {
                            g.set_span(span);
                            let s = relocate_token_stream(g.stream(), span);
                            *g = Group::new(g.delimiter(), s);
                        }
                        TokenTree::Ident(i) => i.set_span(span),
                        TokenTree::Punct(p) => p.set_span(span),
                        TokenTree::Literal(l) => l.set_span(span),
                    };
                    tt
                })
                .collect()
        }

        relocate_token_stream(self.conf.muttest_api.clone(), span)
    }
}

fn csv_quote(s: &str) -> String {
    if s.contains('\"') {
        format!(r#""{}""#, s.replace('"', r#""""#))
    } else {
        s.to_owned()
    }
}

pub(crate) struct TransformSnippets {
    pub(crate) m_id: TokenStream,
    pub(crate) muttest_api: TokenStream,
    pub(crate) loc: TokenStream,
}

#[cfg(procmacro2_semver_exempt)]
fn source_file_path(span: Span) -> Option<PathBuf> {
    Some(span.source_file().path())
}
#[cfg(not(procmacro2_semver_exempt))]
#[allow(unused_variables)]
fn source_file_path(span: Span) -> Option<PathBuf> {
    None
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    #[test]
    fn correct_spans_extreme() {
        #[muttest_codegen::mutate_isolated]
        fn f() {}
        let report = call_isolated! {f()}.report;
        assert_eq!(report.attrs.len(), 1);
        let attr = report.attrs.values().next().unwrap();
        assert!(attr.span.is_some());

        assert_eq!(report.mutables.len(), 1);
        let mutable = report.mutables.values().next().unwrap();

        assert_ne!(mutable.location.span, attr.span);

        assert_eq!(
            mutable.location.span.unwrap().start.line,
            attr.span.unwrap().start.line + 1
        );
    }

    #[test]
    fn correct_spans_ints() {
        #[muttest_codegen::mutate_isolated("lit_int")]
        fn f() -> isize {
            1 + 2 + 3
        }
        let report = call_isolated! {f()}.report;
        assert_eq!(report.attrs.len(), 1);
        let attr = report.attrs.values().next().unwrap();
        assert!(attr.span.is_some());

        assert_eq!(report.mutables.len(), 3);
        let first_col = report
            .mutables
            .values()
            .next()
            .unwrap()
            .location
            .span
            .unwrap()
            .start
            .column;

        for (id, m) in &report.mutables {
            assert_eq!(
                m.location.span.unwrap().start.line,
                attr.span.unwrap().start.line + 2
            );
            assert_eq!(
                m.location.span.unwrap().start.column,
                first_col + 4 * (id.id as u32 - 1)
            );
        }
    }
}
