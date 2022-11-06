use std::{
    collections::{BTreeMap, BTreeSet},
    fmt,
    io::Read,
    path::PathBuf,
    str::FromStr,
    time::Duration,
};

use serde::{Deserialize, Serialize};

use crate::{api::CrateId, parse_or_none_if_empty, CrateLocalMutableId, Error};

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub struct MuttestReport {
    // TODO: new type, something like `CrateId`?
    pub muttest_crates: BTreeMap<CrateId, MuttestReportForCrate>,
    pub test_bins: Vec<TestBin>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct TestBin {
    pub path: PathBuf,
    pub name: String,
    pub exec_time: Option<Duration>,
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub struct MuttestReportForCrate {
    pub attrs: BTreeMap<usize, MutateAttrLocation>,
    pub mutables: BTreeMap<CrateLocalMutableId, MutableReport>,
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub struct MutateAttrLocation {
    pub file: Option<String>,
    pub span: Option<Span>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct MutableReport {
    pub analysis: MutableAnalysis,
    pub results: BTreeMap<String, MutationResult>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct MutableAnalysis {
    pub kind: String,
    pub code: String,
    pub module: Option<String>,
    pub span: Option<Span>,
    pub path: Vec<PathSegment>,
    pub ty: Option<String>,
    pub mutations: Option<Vec<String>>,
    pub covered: bool,
    pub behavior: Option<BTreeSet<String>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum MutationResult {
    IdenticalBehavior,
    Survived,
    Killed,
    Timeout,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub struct MutableLocation<'a> {
    pub file: Option<&'a str>,
    pub module: Option<&'a str>,
    pub span: Option<Span>,
    pub path: &'a [PathSegment],
}

impl From<MutableAnalysis> for MutableReport {
    fn from(analysis: MutableAnalysis) -> Self {
        Self {
            analysis,
            results: Default::default(),
        }
    }
}

// TODO: use proc-macro2 structs instead??
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub struct Span {
    pub start: LineColumn,
    pub end: Option<LineColumn>,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub struct LineColumn {
    pub line: u32,
    pub column: u32,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum PathSegment {
    Mod(String),
    Fn(String),
    Impl(String),
}
impl MuttestReportForCrate {
    pub fn location_of(&self, m_id: CrateLocalMutableId) -> Option<MutableLocation<'_>> {
        let attr = self.attrs.get(&m_id.attr_id)?;
        let m = self.mutables.get(&m_id)?;

        Some(MutableLocation {
            file: attr.file.as_deref(),
            module: m.analysis.module.as_deref(),
            span: m.analysis.span,
            path: &*m.analysis.path,
        })
    }
    pub fn from_definition_csv(definitions: impl Read) -> Result<Self, Error> {
        #[derive(Debug, Deserialize)]
        struct MutableDefinitionCsvLine {
            attr_id: usize,
            id: usize,
            kind: String,
            code: String,
            file: String,
            path: String,
            attr_span: String,
            span: String,
        }

        let mut report = Self::default();

        let mut reader = csv::ReaderBuilder::new().from_reader(definitions);
        for md in reader.deserialize::<MutableDefinitionCsvLine>() {
            let md = md?;
            report.mutables.insert(
                CrateLocalMutableId {
                    attr_id: md.attr_id,
                    id: md.id,
                },
                MutableReport::from(MutableAnalysis {
                    kind: md.kind,
                    code: md.code,
                    module: None,
                    span: parse_or_none_if_empty(&md.span)?,
                    path: md
                        .path
                        .split(':')
                        .map(|s| s.parse())
                        .collect::<Result<_, _>>()?,
                    ty: None,
                    mutations: None,
                    covered: false,
                    behavior: None,
                }),
            );
            let attr = report.attrs.entry(md.attr_id).or_default();
            if !md.file.is_empty() {
                attr.file = Some(md.file);
            }
            if !md.attr_span.is_empty() {
                attr.span = Some(md.attr_span.parse()?);
            }
        }
        Ok(report)
    }

    pub fn read_details_csv(&mut self, details: impl Read) -> Result<(), Error> {
        #[derive(Debug, Deserialize)]
        struct MutableDetailsCsvLine {
            attr_id: usize,
            id: usize,
            ty: String,
            mutations: String,
            file: String,
            module: String,
            attr_span: String,
            span: String,
        }

        let mut reader = csv::ReaderBuilder::new().from_reader(details);
        for md in reader.deserialize::<MutableDetailsCsvLine>() {
            let md = md?;

            let id = CrateLocalMutableId {
                attr_id: md.attr_id,
                id: md.id,
            };
            let mutable = self
                .mutables
                .get_mut(&id)
                .ok_or(Error::UnknownCrateLocalMutableId(id))?;
            let analysis = &mut mutable.analysis;
            // TODO: debug assertions that no info is overwritten incorrectly

            analysis.ty = if md.ty.is_empty() { None } else { Some(md.ty) };
            analysis.mutations = Some(if md.mutations.is_empty() {
                vec![]
            } else {
                md.mutations.split(':').map(ToOwned::to_owned).collect()
            });
            if analysis.span.is_none() {
                analysis.span = parse_or_none_if_empty(&md.span)?;
            }
            if analysis.module.is_none() {
                analysis.module = Some(md.module);
            }

            let attr = self
                .attrs
                .get_mut(&md.attr_id)
                .ok_or_else(|| Error::UnknownMutateAttr(md.attr_id))?;
            if attr.file.is_none() {
                attr.file = Some(md.file);
            }
            if attr.span.is_none() {
                attr.span = Some(md.attr_span.parse()?);
            }
        }
        Ok(())
    }

    pub fn read_coverage_csv(&mut self, coverage: impl Read) -> Result<(), Error> {
        #[derive(Debug, Deserialize)]
        struct MutableCoverageCsvLine {
            attr_id: usize,
            id: usize,
            behavior: String,
        }

        let mut reader = csv::ReaderBuilder::new().from_reader(coverage);
        for md in reader.deserialize::<MutableCoverageCsvLine>() {
            let md = md?;
            // TODO: what to do with pkg_name & crate_name?

            let id = CrateLocalMutableId {
                attr_id: md.attr_id,
                id: md.id,
            };

            let analysis = &mut self
                .mutables
                .get_mut(&id)
                .ok_or(Error::UnknownCrateLocalMutableId(id))?
                .analysis;

            analysis.covered = true;
            analysis.behavior = Some(if md.behavior.is_empty() {
                BTreeSet::new()
            } else {
                md.behavior.split(':').map(ToOwned::to_owned).collect()
            });
        }
        Ok(())
    }
}

impl fmt::Display for MutableLocation<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(file) = self.file {
            write!(f, "{file}")?;
        }
        if self.file.is_some() && self.span.is_some() {
            write!(f, ":")?;
        }
        if let Some(span) = self.span {
            write!(f, "{span}")?;
        }
        // TODO: maybe also write module & path?
        Ok(())
    }
}

impl fmt::Display for PathSegment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PathSegment::Mod(i) => write!(f, "mod {i}"),
            PathSegment::Fn(i) => write!(f, "fn {i}"),
            PathSegment::Impl(i) => write!(f, "impl {i}"),
        }
    }
}
impl FromStr for PathSegment {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.split_once(' ') {
            Some(("mod", i)) => Ok(Self::Mod(i.to_owned())),
            Some(("fn", i)) => Ok(Self::Fn(i.to_owned())),
            Some(("impl", i)) => Ok(Self::Impl(i.to_owned())),
            _ => Err(Error::PathFormat(s.to_owned())),
        }
    }
}

impl FromStr for Span {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let start;
        let mut end = None;
        match s.split_once('-') {
            Some((s, e)) => {
                start = s.parse()?;
                end = Some(e.parse()?);
            }
            None => {
                start = s.parse()?;
            }
        }

        Ok(Self { start, end })
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.start.line, self.start.column)?;
        if let Some(end) = self.end {
            write!(f, "-{}:{}", end.line, end.column)?;
        }
        Ok(())
    }
}

impl FromStr for LineColumn {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (l, c) = s
            .split_once(':')
            .ok_or_else(|| Error::LocationFormat(s.to_owned()))?;
        Ok(LineColumn {
            line: l.parse().map_err(|_| Error::LocationFormat(s.to_owned()))?,
            column: c.parse().map_err(|_| Error::LocationFormat(s.to_owned()))?,
        })
    }
}
