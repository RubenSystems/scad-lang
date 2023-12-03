use std::collections::HashMap;

use super::tir_types::{MonoType, PolyType, TIRType};

#[derive(Debug, Clone)]
pub struct Substitution {
    from_to: HashMap<String, MonoType>,
}

impl Substitution {
    pub fn new() -> Self {
        Self {
            from_to: HashMap::new(),
        }
    }

    pub fn add_sub(&mut self, from: String, to: MonoType) {
        self.from_to.insert(from, to);
    }

    pub fn merge(&self, other: &Self) -> Self {
        let mut self_copy = self.from_to.clone();
        self_copy.extend(other.from_to.clone());
        Substitution { from_to: self_copy }
    }

    pub fn substitute(&self, tpe: &TIRType) -> TIRType {
        match tpe {
            TIRType::MonoType(m) => TIRType::MonoType(self.substitute_mono(m)),
            TIRType::PolyType(p) => TIRType::PolyType(self.substitute_poly(p)),
        }
    }

    pub fn substitute_mono(&self, tpe: &MonoType) -> MonoType {
        match tpe {
            MonoType::Variable(v) => match self.from_to.get(v) {
                Some(s) => s.clone(),
                None => MonoType::Variable(v.clone()),
            },
            MonoType::Application { c, types } => MonoType::Application {
                c: c.clone(),
                types: types.iter().map(|x| self.substitute_mono(x)).collect(),
            },
        }
    }

    pub fn substitute_poly(&self, tpe: &PolyType) -> PolyType {
        match tpe {
            PolyType::MonoType(m) => PolyType::MonoType(self.substitute_mono(m)),
            PolyType::TypeQuantifier { alpha, sigma } => PolyType::TypeQuantifier {
                alpha: alpha.clone(),
                sigma: Box::new(self.substitute_poly(&sigma)),
            },
        }
    }
}
