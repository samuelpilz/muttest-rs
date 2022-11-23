use std::{fmt, str::FromStr};

use serde::Serialize;

use crate::Error;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BakedMutableId {
    pub pkg_name: &'static str,
    pub crate_name: &'static str,
    pub attr_id: usize,
    pub id: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MutableId {
    pub crate_id: CrateId,
    pub id: CrateLocalMutableId,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
#[serde(try_from = "String")]
#[serde(into = "String")]
pub struct CrateId {
    pub pkg_name: String,
    pub crate_name: String,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
#[serde(try_from = "String")]
#[serde(into = "String")]
pub struct CrateLocalMutableId {
    pub attr_id: usize,
    pub id: usize,
}

impl BakedMutableId {
    pub fn cloned(self) -> MutableId {
        MutableId {
            crate_id: CrateId {
                pkg_name: self.pkg_name.to_owned(),
                crate_name: self.crate_name.to_owned(),
            },
            id: self.crate_local_id(),
        }
    }

    pub fn crate_local_id(&self) -> CrateLocalMutableId {
        CrateLocalMutableId {
            attr_id: self.attr_id,
            id: self.id,
        }
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
        let err1 = |_| Error::MutableIdFormat(s.to_owned());
        let (attr_id, id) = s
            .split_once(':')
            .ok_or_else(|| Error::MutableIdFormat(s.to_owned()))?;
        let attr_id = attr_id.parse().map_err(err1)?;
        let id = id.parse().map_err(err1)?;
        Ok(Self { attr_id, id })
    }
}
impl TryFrom<String> for CrateLocalMutableId {
    type Error = Error;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        value.parse()
    }
}
impl From<CrateLocalMutableId> for String {
    fn from(id: CrateLocalMutableId) -> Self {
        id.to_string()
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
            .ok_or_else(|| Error::MutableIdFormat(s.to_owned()))?;
        Ok(Self {
            pkg_name: pkg_name.to_owned(),
            crate_name: crate_name.to_owned(),
        })
    }
}
impl TryFrom<String> for CrateId {
    type Error = Error;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        value.parse()
    }
}
impl From<CrateId> for String {
    fn from(id: CrateId) -> Self {
        id.to_string()
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
        let err1 = |_| Error::MutableIdFormat(s.to_owned());
        let (pkg_name, rest) = s
            .split_once(':')
            .ok_or_else(|| Error::MutableIdFormat(s.to_owned()))?;
        let (crate_name, rest) = rest
            .split_once(':')
            .ok_or_else(|| Error::MutableIdFormat(s.to_owned()))?;
        let id = rest.parse().map_err(err1)?;
        Ok(Self {
            crate_id: CrateId {
                pkg_name: pkg_name.to_owned(),
                crate_name: crate_name.to_owned(),
            },
            id,
        })
    }
}

impl fmt::Display for BakedMutableId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}:{}:{}:{}",
            self.pkg_name, self.crate_name, self.attr_id, self.id
        )
    }
}
