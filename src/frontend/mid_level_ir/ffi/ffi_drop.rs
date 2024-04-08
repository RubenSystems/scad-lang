use super::ffi_types::{FFIHIRExpr, FFIHIRValue, FFIString, OutData};


fn drop_val(val: *const FFIHIRValue) {
    let val_d = unsafe { &*val };

    match val_d.tag {
        super::ffi_types::FFIHirValueTag::Tensor => {
            let value = unsafe { val_d.value.tensor };

			let vec = unsafe {
                Vec::from_raw_parts(value.vals as *mut FFIHIRValue, value.size, value.size)
            };
            drop(vec);
        }
        super::ffi_types::FFIHirValueTag::Integer => {}
        super::ffi_types::FFIHirValueTag::VariableReference => {}
        super::ffi_types::FFIHirValueTag::FunctionCall => {
            let value = unsafe { val_d.value.function_call };
			let vec = unsafe {
                Vec::from_raw_parts(value.params as *mut FFIHIRValue, value.param_len, value.param_len)
            };
            drop(vec);
        }
        super::ffi_types::FFIHirValueTag::Boolean => {}
        super::ffi_types::FFIHirValueTag::Conditional => {
            let value = unsafe { val_d.value.conditional };
            drop_expr(value.if_arm.block);
            drop_expr(value.else_arm);
            drop_val(value.if_arm.condition);
        }
        super::ffi_types::FFIHirValueTag::Float => {}
        super::ffi_types::FFIHirValueTag::Cast => {
            let value = unsafe { val_d.value.cast };
            drop_val(value.expr);
        }
    }
}

fn drop_expr_ref(ex_d: &FFIHIRExpr) {
    match ex_d.tag {
        super::ffi_types::FFIHIRTag::VariableDecl => {
			let expr = unsafe { ex_d.value.variable_decl };
			drop_val(&expr.e1);
			drop_expr(expr.e2);
		},
        super::ffi_types::FFIHIRTag::Noop => {}
        super::ffi_types::FFIHIRTag::FunctionDecl => {
            let expr = unsafe { ex_d.value.function_decl };
            let vec = unsafe {
                Vec::from_raw_parts(expr.args as *mut FFIString, expr.arg_len, expr.arg_len)
            };
            drop(vec);
            drop_expr(expr.e2);
        }
        super::ffi_types::FFIHIRTag::ForwardFunctionDecl => {
            let expr = unsafe { ex_d.value.forward_function_decl };
            drop_expr(expr.e2)
        }
        super::ffi_types::FFIHIRTag::Return => {
			let expr = unsafe { ex_d.value.ret };
			drop_val(&expr.res)
		}
        super::ffi_types::FFIHIRTag::Yield => {
			let expr = unsafe { ex_d.value.yld };
			drop_val(&expr.res)
		}
        super::ffi_types::FFIHIRTag::ForLoop => {
            let expr = unsafe { ex_d.value.floop };
            drop_expr(expr.block);
            drop_expr(expr.e2);
        }
        super::ffi_types::FFIHIRTag::WhileLoop => {
            let expr = unsafe { ex_d.value.whl };
            drop_expr(expr.block);
            drop_expr(expr.cond_expr);
            drop_expr(expr.e2);
        }
    }
}

fn drop_expr(ex: *const FFIHIRExpr) {
    let ex_d = unsafe { &*ex };
    drop_expr_ref(ex_d);
}

pub fn drop_ffi_data(ffi: OutData) {
    if !ffi.compiled {
        return;
    }
    let program = ffi.program;

    drop_expr_ref(unsafe { &program.program });
	drop(unsafe{std::mem::ManuallyDrop::into_inner(program.program)});
}
