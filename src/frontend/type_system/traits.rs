use std::collections::HashMap;

use super::tir_types::MonoType;

pub trait FreeVarsGettable {
    fn free_vars(&self) -> Vec<String>;
}

pub trait Instantiatable {
    fn instantiate(&self, map: &mut HashMap<String, String>) -> MonoType;
}
