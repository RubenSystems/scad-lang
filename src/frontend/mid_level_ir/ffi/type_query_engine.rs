
fn flatten_applications(tpe: MonoType) -> FFIType {
    let MonoType::Application {
        c,
        dimensions,
        types,
    } = tpe
    else {
        unreachable!("Unable to type value")
    };
    let mut flattened = Vec::new();
    collapse_application(
        MonoType::Application {
            c: c.clone(),
            dimensions: dimensions.clone(),
            types: types.clone(),
        },
        &mut flattened,
    );

    let apps: Vec<FFIApplication> = flattened
        .into_iter()
        .map(|x| convert_monotype_to_ffi(x))
        .collect();

    let apps_len = apps.len();
    let apps_ptr = apps.as_ptr();
    std::mem::forget(apps);

    FFIType {
        size: apps_len,
        applications: apps_ptr,
    }
}


fn collapse_application(app: MonoType, collapsed_val: &mut Vec<MonoType>) -> bool {
    let MonoType::Application {
        c,
        dimensions,
        types,
    } = app
    else {
        return false;
    };

    if c == "->" {
        // its not a leaf
        collapse_application(types.first().unwrap().clone(), collapsed_val)
            && collapse_application(types.last().unwrap().clone(), collapsed_val)
    } else {
        collapsed_val.push(MonoType::Application {
            c,
            dimensions,
            types,
        });
        true
    }
}

fn convert_type_to_ffi(tpe: TIRType) -> FFIType {
    match tpe.clone() {
        TIRType::MonoType(mt) => flatten_applications(mt),
        TIRType::PolyType(_) => flatten_applications(instantiate(tpe)),
        TIRType::ForwardDecleration(_) => todo!(),
    }
}

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