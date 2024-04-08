use crate::frontend::type_system::context::Context;

use super::{ffi_conversion::convert_type_to_ffi, ffi_types::FFIType};

pub struct TypeQueryEngine {
    context: Context,
}

impl TypeQueryEngine {
    pub fn new(context: Context) -> Self {
        TypeQueryEngine { context }
    }

    pub fn get_type_for(&self, value: &str) -> FFIType {
        match self.context.env.get(value) {
            Some(v) => convert_type_to_ffi(v.first().unwrap().clone()),
            None => todo!(),
        }
    }
}
