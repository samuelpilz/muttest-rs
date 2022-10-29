use std::{
    borrow::Borrow,
    io::Write,
    path::PathBuf,
    sync::atomic::{AtomicUsize, Ordering::SeqCst},
};

use proc_macro2::{Span, TokenStream};
use quote::quote_spanned;

use crate::{display_or_empty_if_none, mutable::Mutable, BakedMutableId, PathSegment};

pub struct MuttestTransformer<'a, W: Write> {
    pub conf: TransformerConf,
    definitions: Option<W>,
    id: &'a AtomicUsize,
    // TODO: parsed path
    pub path: Vec<PathSegment>,
}
pub struct TransformerConf {
    pub span: Span,
    pub mutables: MutablesConf,
    pub muttest_api: TokenStream,
    pub target_name: &'static str,
}
pub enum MutablesConf {
    All,
    One(String),
}
impl<'a, W: Write> MuttestTransformer<'a, W> {
    pub fn new(conf: TransformerConf, definitions: Option<W>, id: &'a AtomicUsize) -> Self {
        Self {
            conf,
            definitions,
            id,
            path: vec![],
        }
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
        let id = BakedMutableId::new(self.id.fetch_add(1, SeqCst), self.conf.target_name);
        if let Some(w) = &mut self.definitions {
            write_mutable(w, id, m, code, &self.path, self.conf.span);
        }

        let muttest_api = self.conf.muttest_api.clone();
        let crate_name: &str = id.crate_name.borrow();
        let id = id.id;

        let m_id = quote_spanned! {m.span() =>
            #muttest_api::BakedMutableId {id: #id, crate_name: #crate_name}
        };
        let span = bake_span(&muttest_api, m.span());
        let attr_span = bake_span(&muttest_api, self.conf.span);

        let loc = quote_spanned! {m.span()=>
            #muttest_api::BakedLocation {
                file: file!(),
                module: module_path!(),
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
            start: #muttest_api::LineColumn {line: line!(), column: column!() },
            end: #muttest_api::Option::None,
        }
    }
}

/// register a new mutable
fn write_mutable<'a, M: Mutable<'a>, W: Write>(
    mut w: W,
    id: BakedMutableId,
    m: &M,
    code: &str,
    path: &[PathSegment],
    attr_span: Span,
) {
    let span = m.span();
    writeln!(
        w,
        r#"{},{},{},{},{},{},{}"#,
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
    .expect("unable to write mutable file");
    w.flush().expect("unable to flush mutable file");
}

// TODO: streaming display instead of copy
fn csv_quote(s: &str) -> String {
    if s.contains("\"") {
        format!(r#""{}""#, s.replace("\"", "\"\""))
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
