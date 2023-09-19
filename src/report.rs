use std::{
    borrow::Cow,
    collections::{BTreeMap, BTreeSet},
    fmt::{self, Display},
    io::Read,
    path::PathBuf,
    str::FromStr,
    time::Duration,
};

use serde::{Deserialize, Serialize};

use crate::{
    context::{
        AttrLocationCsvLine, LocationCsvLine, MutableCoverageCsvLine, MutableDefinitionCsvLine,
        MutableTypesCsvLine,
    },
    mutator::{identical_behavior, valid_mutations},
    Error,
};

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub struct MuttestReport {
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
    pub attrs: BTreeMap<u32, MutateAttrLocation>,
    pub mutables: BTreeMap<CrateLocalMutableId, MutableReport>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Serialize)]
pub struct MutateAttrLocation {
    pub file: String,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct MutableReport {
    pub kind: String,
    pub analysis: MutableAnalysis,
    pub location: MutableLocation,
    pub mutations: BTreeMap<String, MutationReport>,
}
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct MutableAnalysis {
    pub code: String,
    pub ty: Option<String>,
    pub mutations: Option<Vec<String>>,
    pub behavior: BTreeMap<Option<TestId>, BTreeSet<String>>,
}

#[derive(Debug, Default, Clone, PartialEq, Eq, Serialize)]
pub struct MutationReport {
    pub result: Option<MutationResult>,
    pub relevant_tests: Vec<Option<TestId>>,
}

#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Serialize)]
pub struct ReportSummary {
    pub num_mutables: u32,
    pub num_mutables_covered: u32,
    pub num_mutations: u32,
    pub num_mutations_covered: u32,
    pub num_mutations_killed_weak: u32,
    pub num_mutations_killed: u32,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub enum MutationResult {
    IdenticalBehavior,
    Survived,
    KilledByTestFail,
    KilledByTimeout,
}

#[derive(Debug, Default, Clone, PartialEq, Eq, Serialize)]
pub struct MutableLocation {
    pub module: Option<String>,
    pub span: Option<Span>,
    pub path: Vec<PathSegment>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(try_from = "String")]
#[serde(into = "String")]

pub struct Span {
    pub start: LineColumn,
    pub end: Option<LineColumn>,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(try_from = "String")]
#[serde(into = "String")]
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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(try_from = "String")]
#[serde(into = "String")]
pub struct MutableId {
    pub crate_id: CrateId,
    pub id: CrateLocalMutableId,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(try_from = "String")]
#[serde(into = "String")]
pub struct CrateId {
    pub pkg_name: Cow<'static, str>,
    pub crate_name: Cow<'static, str>,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(try_from = "String")]
#[serde(into = "String")]
pub struct CrateLocalMutableId {
    pub attr_id: u32,
    pub id: u32,
}
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(try_from = "String")]
#[serde(into = "String")]
pub struct TestId {
    pub crate_id: CrateId,
    pub id: CrateLocalTestId,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(try_from = "String")]
#[serde(into = "String")]
pub struct CrateLocalTestId {
    pub attr_id: u32,
    pub id: u32,
}

impl CrateLocalTestId {
    pub const INVALID: Self = Self { attr_id: 0, id: 0 };
}

impl MutableAnalysis {
    pub fn is_covered(&self) -> bool {
        !self.behavior.is_empty()
    }
}

impl Span {
    // TODO: remove optional-check for end when procmacro has access to locations
    pub fn from_span(span: proc_macro2::Span) -> Option<Self> {
        let start = span.start();
        if start.line == 0 {
            return None;
        }
        let end = Some(span.end()).filter(|&end| end.line != 0 && start != end);
        Some(Self {
            start: LineColumn {
                line: start.line.try_into().ok()?,
                column: start.column.try_into().ok()?,
            },
            end: end.and_then(|end| {
                Some(LineColumn {
                    line: end.line.try_into().ok()?,
                    column: end.column.try_into().ok()?,
                })
            }),
        })
    }
}

impl fmt::Display for CrateLocalMutableId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.attr_id, self.id)
    }
}
impl FromStr for CrateLocalMutableId {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let err = || Error::IdFormat(s.to_owned());
        let err1 = |_| err();
        let (attr_id, id) = s.split_once(':').ok_or_else(err)?;
        let attr_id = attr_id.parse().map_err(err1)?;
        let id = id.parse().map_err(err1)?;
        Ok(Self { attr_id, id })
    }
}
impl fmt::Display for TestId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.crate_id, self.id)
    }
}
impl FromStr for TestId {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let err = || Error::IdFormat(s.to_owned());
        let err1 = |_| err();
        let (pkg_name, rest) = s.split_once(':').ok_or_else(err)?;
        let (crate_name, rest) = rest.split_once(':').ok_or_else(err)?;
        let id = rest.parse().map_err(err1)?;
        Ok(Self {
            crate_id: CrateId {
                pkg_name: Cow::Owned(pkg_name.to_owned()),
                crate_name: Cow::Owned(crate_name.to_owned()),
            },
            id,
        })
    }
}

impl fmt::Display for CrateLocalTestId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.attr_id, self.id)
    }
}
impl FromStr for CrateLocalTestId {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let err = || Error::IdFormat(s.to_owned());
        let err1 = |_| err();
        let (attr_id, id) = s.split_once(':').ok_or_else(err)?;
        let attr_id = attr_id.parse().map_err(err1)?;
        let id = id.parse().map_err(err1)?;
        Ok(Self { attr_id, id })
    }
}

impl fmt::Display for CrateId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.pkg_name, self.crate_name)
    }
}
impl FromStr for CrateId {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (pkg_name, crate_name) = s
            .split_once(':')
            .ok_or_else(|| Error::IdFormat(s.to_owned()))?;
        Ok(Self {
            pkg_name: Cow::Owned(pkg_name.to_owned()),
            crate_name: Cow::Owned(crate_name.to_owned()),
        })
    }
}
impl fmt::Display for MutableId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.crate_id, self.id)
    }
}
impl FromStr for MutableId {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let err = || Error::IdFormat(s.to_owned());
        let err1 = |_| err();
        let (pkg_name, rest) = s.split_once(':').ok_or_else(err)?;
        let (crate_name, rest) = rest.split_once(':').ok_or_else(err)?;
        let id = rest.parse().map_err(err1)?;
        Ok(Self {
            crate_id: CrateId {
                pkg_name: Cow::Owned(pkg_name.to_owned()),
                crate_name: Cow::Owned(crate_name.to_owned()),
            },
            id,
        })
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
        write!(f, "{}", self.start)?;
        if let Some(end) = self.end {
            write!(f, "-{}", end)?;
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

impl Display for LineColumn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.column)?;
        Ok(())
    }
}

impl MuttestReportForCrate {
    pub fn from_definition_csv(definitions: impl Read) -> Result<Self, Error> {
        let mut report = Self::default();

        let mut reader = csv::ReaderBuilder::new().from_reader(definitions);
        for md in reader.deserialize::<MutableDefinitionCsvLine>() {
            let md = md?;
            report.mutables.insert(
                md.mut_id,
                MutableReport {
                    kind: md.kind,
                    analysis: MutableAnalysis {
                        code: md.code,
                        ty: Some(md.type_info).filter(|s| !s.is_empty()),
                        mutations: None,
                        behavior: BTreeMap::default(),
                    },
                    location: Default::default(),
                    mutations: Default::default(),
                },
            );
        }
        Ok(report)
    }

    fn weak_mutation_analysis(&mut self) -> Result<(), Error> {
        for mutable in self.mutables.values_mut() {
            mutable.weak_mutation_analysis()?;
        }
        Ok(())
    }

    pub fn summary(&self) -> ReportSummary {
        let mut summary = ReportSummary::default();
        for mutable in self.mutables.values() {
            summary.num_mutables += 1;
            if mutable.analysis.is_covered() {
                summary.num_mutables_covered += 1;
            }
            for m in mutable.mutations.values() {
                summary.num_mutations += 1;
                if mutable.analysis.is_covered() {
                    summary.num_mutations_covered += 1;
                }
                if m.result != Some(MutationResult::IdenticalBehavior) {
                    summary.num_mutations_killed_weak += 1;
                }
                if matches!(
                    m.result,
                    Some(MutationResult::KilledByTestFail | MutationResult::KilledByTimeout)
                ) {
                    summary.num_mutations_killed += 1;
                }
            }
        }
        summary
    }

    pub fn num_covered(&self) -> u32 {
        self.mutables
            .values()
            .filter(|m| m.analysis.is_covered())
            .count()
            .try_into()
            .unwrap()
    }

    pub fn num_survived_weak(&self) -> u32 {
        self.mutables
            .values()
            .flat_map(|m| m.mutations.values())
            .filter(|m| m.result == Some(MutationResult::IdenticalBehavior))
            .count()
            .try_into()
            .unwrap()
    }

    pub fn filter_files(&mut self, files: &[String]) {
        self.mutables.retain(|id, _| {
            let file = &self.attrs[&id.attr_id].file;
            files.iter().any(|f| file.contains(f))
        });
    }
    pub fn filter_mutators<F: Fn(&str) -> bool>(&mut self, predicate: F) {
        self.mutables.retain(|_, m| predicate(&m.kind));
    }
}

impl MuttestReport {
    /// Perform weak mutation analysis.
    ///
    /// This assumes that the files for types and coverage have been read.
    pub fn weak_mutation_analysis(&mut self) -> Result<(), Error> {
        for report in self.muttest_crates.values_mut() {
            report.weak_mutation_analysis()?;
        }
        Ok(())
    }

    /// Remove non-matching files from the report.
    pub fn filter_files(&mut self, files: &[String]) {
        for report in self.muttest_crates.values_mut() {
            report.filter_files(files);
        }
    }

    /// Only keep mutators matching the predicate
    pub fn filter_mutators<F: Fn(&str) -> bool>(&mut self, predicate: F) {
        for report in self.muttest_crates.values_mut() {
            report.filter_mutators(&predicate);
        }
    }

    pub fn read_attr_location_csv(&mut self, csv: impl Read) -> Result<(), Error> {
        let mut reader = csv::ReaderBuilder::new().from_reader(csv);
        for line in reader.deserialize::<AttrLocationCsvLine>() {
            let AttrLocationCsvLine {
                crate_id,
                id,
                file,
                span,
            } = line?;
            let crate_report = self
                .muttest_crates
                .get_mut(&crate_id)
                .ok_or_else(|| Error::UnknownCrateId(crate_id.clone()))?;

            crate_report
                .attrs
                .insert(id, MutateAttrLocation { file, span });
        }
        Ok(())
    }
    pub fn read_location_csv(&mut self, csv: impl Read) -> Result<(), Error> {
        let mut reader = csv::ReaderBuilder::new().from_reader(csv);
        for line in reader.deserialize::<LocationCsvLine>() {
            let LocationCsvLine { mut_id, span } = line?;
            let crate_report = self
                .muttest_crates
                .get_mut(&mut_id.crate_id)
                .ok_or_else(|| Error::UnknownCrateId(mut_id.crate_id.clone()))?;

            let mutable = crate_report
                .mutables
                .get_mut(&mut_id.id)
                .ok_or_else(|| Error::UnknownMutableId(mut_id.clone()))?;
            mutable.location.span = Some(span);
        }
        Ok(())
    }
    pub fn read_types_csv(&mut self, csv: impl Read) -> Result<(), Error> {
        let mut reader = csv::ReaderBuilder::new().from_reader(csv);
        for line in reader.deserialize::<MutableTypesCsvLine>() {
            let line = line?;

            let crate_report = self
                .muttest_crates
                .get_mut(&line.mut_id.crate_id)
                .ok_or_else(|| Error::UnknownCrateId(line.mut_id.crate_id.clone()))?;

            let mutable = crate_report
                .mutables
                .get_mut(&line.mut_id.id)
                .ok_or_else(|| Error::UnknownMutableId(line.mut_id.clone()))?;

            // update analysis
            let analysis = &mut mutable.analysis;
            if !line.ty.is_empty() {
                analysis.ty = Some(line.ty);
            }
            analysis.mutations = Some(if line.mutations.is_empty() {
                vec![]
            } else {
                line.mutations
                    .split('\x1f')
                    .map(ToOwned::to_owned)
                    .collect()
            });
        }
        Ok(())
    }

    pub fn read_coverage_csv(&mut self, coverage: impl Read) -> Result<(), Error> {
        let mut reader = csv::ReaderBuilder::new().from_reader(coverage);
        for line in reader.deserialize::<MutableCoverageCsvLine>() {
            let line = line?;

            let crate_report = self
                .muttest_crates
                .get_mut(&line.mut_id.crate_id)
                .ok_or_else(|| Error::UnknownCrateId(line.mut_id.crate_id.clone()))?;

            let analysis = &mut crate_report
                .mutables
                .get_mut(&line.mut_id.id)
                .ok_or_else(|| Error::UnknownMutableId(line.mut_id.clone()))?
                .analysis;

            *analysis.behavior.entry(line.test_id).or_default() = if line.behavior.is_empty() {
                BTreeSet::new()
            } else {
                line.behavior.split('\x1f').map(ToOwned::to_owned).collect()
            };
        }
        Ok(())
    }
}

impl MutableReport {
    fn weak_mutation_analysis(&mut self) -> Result<(), Error> {
        debug_assert!(self.mutations.is_empty());
        for m in valid_mutations(&self.kind, &self.analysis)? {
            let relevant_tests = self
                .analysis
                .behavior
                .iter()
                .filter(|(_, b)| {
                    !identical_behavior(&self.kind, &self.analysis.code, b, &m).unwrap()
                })
                .map(|(t, _)| t.clone())
                .collect::<Vec<_>>();
            let report = MutationReport {
                result: relevant_tests
                    .is_empty()
                    .then_some(MutationResult::IdenticalBehavior),
                relevant_tests,
            };
            self.mutations.insert(m, report);
        }

        Ok(())
    }
}

impl ReportSummary {
    pub fn ratio_mutables_covered(self) -> Ratio {
        Ratio(self.num_mutables_covered, self.num_mutables)
    }
    pub fn ratio_mutations_covered(self) -> Ratio {
        Ratio(self.num_mutations_covered, self.num_mutations)
    }
    pub fn ratio_mutations_killed_weak(self) -> Ratio {
        Ratio(self.num_mutations_killed_weak, self.num_mutations)
    }
    pub fn ratio_mutations_killed(self) -> Ratio {
        Ratio(self.num_mutations_killed, self.num_mutations)
    }
}

impl std::ops::AddAssign for ReportSummary {
    fn add_assign(&mut self, rhs: Self) {
        self.num_mutables += rhs.num_mutables;
        self.num_mutables_covered += rhs.num_mutables_covered;
        self.num_mutations += rhs.num_mutations;
        self.num_mutations_covered += rhs.num_mutations_covered;
        self.num_mutations_killed_weak += rhs.num_mutations_killed_weak;
        self.num_mutations_killed += rhs.num_mutations_killed;
    }
}

impl std::ops::Add for ReportSummary {
    type Output = Self;

    fn add(mut self, rhs: Self) -> Self {
        self += rhs;
        self
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Ratio(u32, u32);
impl fmt::Display for Ratio {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}/{} ({:.2}%)", self.0, self.1, self.percent())
    }
}
impl Ratio {
    pub fn percent(self) -> f64 {
        100.0 * self.0 as f64 / self.1 as f64
    }
}

// conversion traits for serde impls
macro_rules! try_from_and_into {
    ($t:ident) => {
        impl TryFrom<String> for $t {
            type Error = Error;

            fn try_from(value: String) -> Result<Self, Self::Error> {
                value.parse()
            }
        }
        impl From<$t> for String {
            fn from(id: $t) -> Self {
                id.to_string()
            }
        }
    };
}
try_from_and_into!(CrateId);
try_from_and_into!(MutableId);
try_from_and_into!(CrateLocalMutableId);
try_from_and_into!(TestId);
try_from_and_into!(CrateLocalTestId);
try_from_and_into!(Span);
try_from_and_into!(LineColumn);
