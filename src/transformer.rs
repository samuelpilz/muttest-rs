use std::path::PathBuf;

use proc_macro2::{Span, TokenStream};
use quote::{quote, quote_spanned};

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
    pub pkg_name: String,
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
                pkg_name: self.conf.pkg_name.clone(),
                crate_name: self.conf.crate_name.clone(),
            },
            id: CrateLocalMutableId {
                attr_id: self.conf.attr_id,
                id: next_id,
            },
        };
        self.definitions
            .push(write_mutable(id.id, m, code, &self.path, self.conf.span));

        let muttest_api = self.conf.muttest_api.clone();
        let pkg_name = &id.crate_id.pkg_name;
        let crate_name = &id.crate_id.crate_name;
        let attr_id = id.id.attr_id;
        let id = id.id.id;
        let m_id = quote_spanned! {m.span() =>
            #muttest_api::BakedMutableId {
                pkg_name: #pkg_name,
                crate_name: #crate_name,
                attr_id: #attr_id,
                id: #id,
            }
        };
        let attr_span = bake_span(&muttest_api, self.conf.span);
        let span = bake_span(&muttest_api, m.span());

        let loc = quote_spanned! {m.span()=>
            #muttest_api::BakedLocation {
                file: #muttest_api::file!(),
                module: #muttest_api::module_path!(),
                attr_span: #attr_span,
                span: #span,
            }
        };

        TransformSnippets {
            m_id,
            muttest_api,
            loc,
        }
    }
}

fn bake_span(muttest_api: &TokenStream, span: Span) -> TokenStream {
    // TODO: use Span::before/after when available to get the entire range
    quote_spanned! {span=>
        #muttest_api::Span {
            start: #muttest_api::LineColumn {line: #muttest_api::line!(), column: #muttest_api::column!() },
            end: #muttest_api::Option::None,
        }
    }
}

/// register a new mutable
fn write_mutable<'a, M: Mutable<'a>>(
    id: CrateLocalMutableId,
    m: &M,
    code: &str,
    path: &[PathSegment],
    attr_span: Span,
) -> String {
    let span = m.span();
    format!(
        r#"{},{},{},{},{},{},{},{}"#,
        id.attr_id,
        id.id,
        M::NAME,
        csv_quote(code),
        source_file_path(span).unwrap_or_default().display(),
        path.iter()
            .map(ToString::to_string)
            .collect::<Vec<_>>()
            .join(":"),
        display_or_empty_if_none(&crate::Span::from(span)),
        display_or_empty_if_none(&crate::Span::from(attr_span)),
    )
}

// TODO: streaming display instead of copy
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

// TODO: tests

#[cfg(test)]
mod tests {
    use crate::tests::*;

    #[test]
    fn correct_spans() {
        #[muttest_codegen::mutate_isolated("extreme")]
        fn f_1234() {
            1;
        }

        // let report = call_isolated! {f()}.report;
        // assert_eq!(report.attrs.len(), 1);
        // let attr = report.attrs.iter().next().unwrap().1;
        // assert!(attr.span.is_some());

        // for m in report.mutables.values() {
        //     assert_ne!(m.location.span, attr.span)
        // }
    }
}
