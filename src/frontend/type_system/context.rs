//===----------------------------------------------------------------------===//
///
/// Definiton of the typing enviornment (capital gamma?) which is just 
/// a lookup table which maps functions and variables to the correct type 
///
//===----------------------------------------------------------------------===//



use std::collections::HashMap;

use super::{substitution::Substitution, tir_types::TIRType, traits::FreeVarsGettable};

#[derive(Clone, Debug)]
pub struct Context {
    // Vec allows for polymorphism
    pub env: HashMap<String, Vec<TIRType>>,
}

impl Context {
    pub fn new() -> Self {
        Self {
            env: HashMap::new(),
        }
    }

    pub fn add_type_for_name(&mut self, name: String, tpe: TIRType) {
        // self.env.insert(name, tpe);
        let mut new_env = self.env.remove(&name).unwrap_or(vec![]);
        new_env.push(tpe);
        self.env.insert(name, new_env);
    }

    pub fn remove_type_for_name(&mut self, name: &str) {
        self.env.remove(name);
    }

    pub fn get_type_for_name(&self, name: &str) -> Option<&Vec<TIRType>> {
        self.env.get(name)
    }

    pub fn has_type_for_name(&self, name: &str) -> bool {
        self.env.contains_key(name)
    }

    pub fn applying_substitution(&self, sub: &Substitution) -> Self {
        Self {
            env: self
                .env
                .iter()
                .map(|(k, v)| (k.clone(), v.iter().map(|tpe| sub.substitute(tpe)).collect()))
                .collect(),
        }
    }
}

impl FreeVarsGettable for Context {
    fn free_vars(&self) -> Vec<String> {
        self.env
            .iter()
            .flat_map(|(_, v)| v.iter().flat_map(|t| t.free_vars()))
            .collect()
    }
}
