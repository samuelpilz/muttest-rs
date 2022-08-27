use std::{
    borrow::{Borrow, Cow},
    fs,
    io::Write,
    ops::DerefMut,
    path::PathBuf,
    sync::{
        atomic::{AtomicUsize, Ordering::SeqCst},
        Mutex,
    },
};

use lazy_static::lazy_static;
use proc_macro2::{Span, TokenStream};
use quote::{format_ident, quote_spanned, ToTokens};

use crate::{Error, MutableId, MUTTEST_DIR};

pub const MUTABLE_DEFINITIONS_CSV_HEAD: &str = "id,kind,code,loc";
static MUTABLE_ID_NUM: AtomicUsize = AtomicUsize::new(1);

lazy_static! {
    static ref TARGET_NAME: String = {
        let mut target_name = std::env::var("CARGO_PKG_NAME").expect("unable to get env var");
        let crate_name = std::env::var("CARGO_CRATE_NAME").expect("unable to get env var");
        if crate_name != target_name {
            target_name.push_str(":");
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

pub struct MuttestTransformer {
    pub conf: TransformerConf,
    pub core_crate: Option<&'static str>,
    pub isolated: Option<TransformerData>,
}
pub struct TransformerConf {
    pub mutables: MutablesConf,
}
pub enum MutablesConf {
    All,
    One(String),
}
pub struct TransformerData {
    pub local_id: usize,
    pub mutables_csv: String,
}
impl MuttestTransformer {
    pub fn new() -> Self {
        Self {
            conf: TransformerConf {
                mutables: MutablesConf::All,
            },
            core_crate: Some("muttest"),
            isolated: None,
        }
    }
    pub fn new_isolated() -> Self {
        Self {
            conf: TransformerConf {
                mutables: MutablesConf::All,
            },
            core_crate: None,
            isolated: Some(TransformerData {
                local_id: 0,
                mutables_csv: format!("{MUTABLE_DEFINITIONS_CSV_HEAD}\n"),
            }),
        }
    }
    pub fn new_selftest() -> Self {
        Self {
            conf: TransformerConf {
                mutables: MutablesConf::All,
            },
            core_crate: None,
            isolated: None,
        }
    }

    pub fn should_mutate(&self, mutable: &str) -> bool {
        match &self.conf.mutables {
            MutablesConf::One(m) => m == mutable,
            MutablesConf::All => true,
        }
    }

    /// register a new mutable
    pub fn register_new_mutable(
        &mut self,
        mut_kind: &str,
        code: &str,
        loc: &str,
    ) -> MutableId<'static> {
        match &mut self.isolated {
            Some(data) => {
                data.local_id += 1;
                let id = data.local_id;

                data.mutables_csv += &format!("{id},{mut_kind},{},{loc}\n", code);

                MutableId {
                    id,
                    crate_name: Cow::Borrowed(""),
                }
            }
            None => {
                let id = MUTABLE_ID_NUM.fetch_add(1, SeqCst);

                let mut f = MUTABLE_DEFINITIONS_FILE.lock().unwrap();
                if let Some(f) = f.deref_mut() {
                    writeln!(f, "{id},{mut_kind},{},{loc}", code)
                        .expect("unable to write mutable file");
                    f.flush().expect("unable to flush mutable file");
                }

                MutableId {
                    id,
                    crate_name: Cow::Borrowed(&*TARGET_NAME),
                }
            }
        }
    }

    pub fn mutable_id_expr(&self, m_id: &MutableId, span: Span) -> TokenStream {
        let id = m_id.id;
        let crate_name: &str = m_id.crate_name.borrow();
        let core_crate = self.core_crate_path(span);

        quote_spanned! {span =>
            #core_crate::MutableId {id: #id, crate_name: ::std::borrow::Cow::Borrowed(#crate_name)}
        }
    }

    // TODO: maybe this can be done with hygiene instead?
    pub fn core_crate_path(&self, span: Span) -> TokenStream {
        match self.core_crate {
            Some(cc) => format_ident!("{}", span = span, cc).into_token_stream(),
            None => quote_spanned! {span => crate},
        }
    }
}
