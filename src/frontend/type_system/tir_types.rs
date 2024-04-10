//===----------------------------------------------------------------------===// 
///
/// Mono and poly type definitions for the typing algorithms
/// 
//===----------------------------------------------------------------------===//


use std::collections::HashMap;

use super::traits::{FreeVarsGettable, Instantiatable};

static mut CURRENT_TMP_INDEX: u128 = 0;

pub fn generate_type_name() -> String {
    let val = unsafe { CURRENT_TMP_INDEX };
    unsafe { CURRENT_TMP_INDEX += 1 };
    format!("t{val}")
}

#[derive(Debug, Clone, Eq)]
pub enum MonoType {
    Variable(String),
    Application {
        c: String,
        dimensions: Option<Vec<u32>>,
        types: Vec<MonoType>,
    },
}

impl PartialEq for MonoType {
    fn eq(&self, other: &Self) -> bool {
        match self {
            MonoType::Variable(_) => true,
            MonoType::Application {
                c,
                dimensions,
                types,
            } => {
                let MonoType::Application {
                    c: o_c,
                    dimensions: o_d,
                    types: o_t,
                } = other
                else {
                    return false;
                };
                c == o_c && dimensions == o_d && types == o_t
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PolyType {
    MonoType(MonoType),
    TypeQuantifier { alpha: String, sigma: Box<PolyType> },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TIRType {
    MonoType(MonoType),
    PolyType(PolyType),
    ForwardDecleration(MonoType),
}

impl Instantiatable for MonoType {
    fn instantiate(&self, map: &mut HashMap<String, String>) -> Self {
        match self {
            MonoType::Variable(vn) => match map.get(vn) {
                Some(s) => MonoType::Variable(s.clone()),
                None => self.clone(),
            },
            MonoType::Application {
                c,
                dimensions,
                types,
            } => MonoType::Application {
                c: c.clone(),
                dimensions: dimensions.clone(),
                types: types.iter().map(|x| x.instantiate(map)).collect(),
            },
        }
    }
}

impl Instantiatable for PolyType {
    fn instantiate(&self, map: &mut HashMap<String, String>) -> MonoType {
        match self {
            PolyType::MonoType(m) => m.instantiate(map),
            PolyType::TypeQuantifier { alpha, sigma } => {
                map.insert(alpha.clone(), generate_type_name());
                sigma.instantiate(map)
            }
        }
    }
}

impl FreeVarsGettable for TIRType {
    fn free_vars(&self) -> Vec<String> {
        match self {
            TIRType::MonoType(m) => m.free_vars(),
            TIRType::PolyType(p) => p.free_vars(),
            TIRType::ForwardDecleration(fd) => fd.free_vars(),
        }
    }
}

impl FreeVarsGettable for MonoType {
    fn free_vars(&self) -> Vec<String> {
        match self {
            MonoType::Variable(v) => vec![v.to_string()],
            MonoType::Application {
                c: _,
                dimensions: _,
                types,
            } => types.iter().flat_map(|x| x.free_vars()).collect(),
        }
    }
}

impl FreeVarsGettable for PolyType {
    fn free_vars(&self) -> Vec<String> {
        match self {
            PolyType::MonoType(mt) => mt.free_vars(),
            PolyType::TypeQuantifier { alpha, sigma } => sigma
                .free_vars()
                .into_iter()
                .filter(|x| x != alpha)
                .collect(),
        }
    }
}
