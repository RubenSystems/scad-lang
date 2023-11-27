use std::collections::HashMap;

use super::{tir_types::TIRType, substitution::Substitution, traits::FreeVarsGettable};

#[derive(Clone, Debug)]
pub struct Context {
    env: HashMap<String, TIRType>,
}

impl Context {
    pub fn new() -> Self {
        Self {
            env: HashMap::new(),
        }
    }

    pub fn add_type_for_name(&mut self, name: String, tpe: TIRType) {
        self.env.insert(name, tpe);
    }

    pub fn get_type_for_name(&self, name: &String) -> Option<&TIRType> {
        self.env.get(name)
    }

    pub fn applying_substitution(&self, sub: &Substitution) -> Self {
        Self {
            env: self
                .env
                .iter()
                .map(|(k, v)| (k.clone(), sub.substitute(v)))
                .collect(),
        }
    }
}


impl FreeVarsGettable for Context {
    fn free_vars(&self) -> Vec<String> {
        self.env.iter().flat_map(|(_, v)| v.free_vars()).collect()
    }
}
