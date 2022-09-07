use std::{
    borrow::Borrow,
    fs,
    io::Write,
    path::{Path, PathBuf},
    sync::{
        atomic::{AtomicUsize, Ordering::SeqCst},
        Mutex,
    },
};

use lazy_static::lazy_static;
use proc_macro2::{Span, TokenStream};
use quote::{format_ident, quote_spanned, ToTokens};

use crate::{Error, MutableId, MUTTEST_DIR};

pub const MUTABLE_DEFINITIONS_CSV_HEAD: &str = "id,kind,code,path,span";
static MUTABLE_ID_NUM: AtomicUsize = AtomicUsize::new(1);

lazy_static! {
    static ref TARGET_NAME: String = {
        let mut target_name = std::env::var("CARGO_PKG_NAME").expect("unable to get env var");
        let crate_name = std::env::var("CARGO_CRATE_NAME").expect("unable to get env var");
        if crate_name != target_name {
            target_name.push(':');
            target_name.push_str(&crate_name);
        }
        target_name
    };
    static ref MUTABLE_DEFINITIONS_FILE: Mutex<Option<fs::File>> =
        Mutex::new(open_definitions_file().expect("unable to open definitions file"));
}

fn open_definitions_file() -> Result<Option<fs::File>, Error> {
    match &*MUTTEST_DIR {
        Some(dir) => {
            let file_name = format!("mutable-definitions-{}.csv", &*TARGET_NAME);
            let mut file = fs::File::create(PathBuf::from(dir).join(file_name))?;
            writeln!(&mut file, "{MUTABLE_DEFINITIONS_CSV_HEAD}")?;
            file.flush()?;
            file.sync_all()?;
            Ok(Some(file))
        }
        _ => Ok(None),
    }
}

pub trait Mutable<'a> {
    const NAME: &'static str;

    fn span(&self) -> Span;

    fn transform(self, transformer: &mut MuttestTransformer) -> TokenStream;
}

pub struct MuttestTransformer {
    pub conf: TransformerConf,
    pub isolated: Option<TransformerData>,
    // TODO: parsed path
    pub path: Vec<String>,
}
pub struct TransformerConf {
    pub mutables: MutablesConf,
    pub muttest_api: Option<&'static str>,
}
pub enum MutablesConf {
    All,
    One(String),
}
pub struct TransformerData {
    pub local_id: usize,
    pub mutables_csv: Vec<u8>,
}
impl Default for MuttestTransformer {
    fn default() -> Self {
        Self::new()
    }
}
impl MuttestTransformer {
    pub fn new() -> Self {
        Self {
            conf: TransformerConf {
                mutables: MutablesConf::All,
                muttest_api: Some("muttest"),
            },
            isolated: None,
            path: vec![],
        }
    }
    pub fn new_isolated() -> Self {
        Self {
            conf: TransformerConf {
                mutables: MutablesConf::All,
                muttest_api: None,
            },
            isolated: Some(TransformerData {
                local_id: 0,
                mutables_csv: format!("{MUTABLE_DEFINITIONS_CSV_HEAD}\n").into_bytes(),
            }),
            path: vec![],
        }
    }
    pub fn new_selftest() -> Self {
        Self {
            conf: TransformerConf {
                mutables: MutablesConf::All,
                muttest_api: None,
            },
            isolated: None,
            path: vec![],
        }
    }

    pub fn should_mutate(&self, mutable: &str) -> bool {
        match &self.conf.mutables {
            MutablesConf::One(m) => m == mutable,
            MutablesConf::All => true,
        }
    }

    pub fn new_mutable<'a, M: Mutable<'a>>(&mut self, m: &M, code: &str) -> TransformSnippets {
        let id;
        match &mut self.isolated {
            Some(data) => {
                id = MutableId::new_isolated({
                    data.local_id += 1;
                    data.local_id
                });
                write_mutable(&self.path, Some(&mut data.mutables_csv), &id, m, code);
            }
            None => {
                id = MutableId::new(MUTABLE_ID_NUM.fetch_add(1, SeqCst), &*TARGET_NAME);
                write_mutable(
                    &self.path,
                    MUTABLE_DEFINITIONS_FILE.lock().unwrap().as_mut(),
                    &id,
                    m,
                    code,
                );
            }
        }

        let muttest_api = match self.conf.muttest_api {
            Some(cc) => format_ident!("{}", span = m.span(), cc).into_token_stream(),
            None => quote_spanned! {m.span() => crate::api},
        };

        let crate_name: &str = id.crate_name.borrow();
        let id = id.id;

        let m_id = quote_spanned! {m.span() =>
            #muttest_api::MutableId {id: #id, crate_name: #muttest_api::Cow::Borrowed(#crate_name)}
        };
        // TODO: use Span::before/after when available to get the entire range
        // TODO: also print location of attribute
        let loc = quote_spanned! {m.span()=>
            #muttest_api::MutableLocation {file: #muttest_api::Cow::Borrowed(file!()), line: line!(), column: column!() }
        };

        TransformSnippets {
            m_id,
            muttest_api,
            loc,
        }
    }
}
/// register a new mutable
fn write_mutable<'a, M: Mutable<'a>, W: Write>(
    path: &[String],
    f: Option<W>,
    id: &MutableId,
    m: &M,
    code: &str,
) {
    if let Some(mut f) = f {
        writeln!(
            f,
            "{},{},{},{},{}",
            id.id,
            M::NAME,
            code,
            path.join(" => "),
            display_span(m.span())
        )
        .expect("unable to write mutable file");
        f.flush().expect("unable to flush mutable file");
    }
}

pub struct TransformSnippets {
    pub m_id: TokenStream,
    pub muttest_api: TokenStream,
    pub loc: TokenStream,
}

pub fn display_span(span: Span) -> String {
    let start = span.start();
    let end = span.end();
    if [start.line, start.column, end.line, end.column] == [0, 0, 0, 0] {
        return String::new();
    }
    format!(
        "{}@{}:{}-{}:{}",
        source_file_path(span)
            .as_deref()
            .unwrap_or_else(|| Path::new("<unknown-file>"))
            .display(),
        start.line,
        start.column,
        end.line,
        end.column
    )
}

#[cfg(procmacro2_semver_exempt)]
pub fn source_file_path(span: Span) -> Option<PathBuf> {
    Some(span.source_file().path())
}
#[cfg(not(procmacro2_semver_exempt))]
#[allow(unused_variables)]
pub fn source_file_path(span: Span) -> Option<PathBuf> {
    None
}
