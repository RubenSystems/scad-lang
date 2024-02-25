use crate::frontend::type_system::{
    context::Context,
    tir_types::{MonoType, PolyType, TIRType},
};

pub fn create_types_for_core() -> Context {
    let mut consumable_context = Context::new();

    consumable_context.add_type_for_name(
        "@print".into(),
        TIRType::MonoType(MonoType::Application {
            dimensions: None,
            c: "->".into(),
            types: vec![
                MonoType::Variable("any_vec_any".into()),
                MonoType::Variable("any_vec_any".into()),
            ],
        }),
    );

    consumable_context.add_type_for_name(
        "@drop".into(),
        TIRType::PolyType(PolyType::TypeQuantifier {
            alpha: "@drop.type".into(),
            sigma: Box::new(PolyType::MonoType(MonoType::Application {
                dimensions: None,
                c: "->".into(),
                types: vec![
                    MonoType::Variable("@drop.type".into()),
                    MonoType::Variable("@drop.type".into()),
                ],
            })),
        }),
    );

    consumable_context.add_type_for_name(
        "@add".into(),
        TIRType::PolyType(PolyType::TypeQuantifier {
            alpha: "@add.type".into(),
            sigma: Box::new(PolyType::MonoType(MonoType::Application {
                c: "->".into(),
                dimensions: None,
                types: vec![
                    MonoType::Variable("@add.type".into()),
                    MonoType::Application {
                        c: "->".into(),
                        dimensions: None,
                        types: vec![
                            MonoType::Variable("@add.type".into()),
                            MonoType::Variable("@add.type".into()),
                        ],
                    },
                ],
            })),
        }),
    );

    // Create types for index!
    vec![8, 16, 32, 64].into_iter().for_each(|x| {
        consumable_context.add_type_for_name(
            format!("@index.i{x}"),
            TIRType::PolyType(PolyType::TypeQuantifier {
                alpha: "@index.tensor".into(),
                sigma: Box::new(PolyType::MonoType(MonoType::Application {
                    c: "->".into(),
                    dimensions: None,
                    types: vec![
                        MonoType::Variable("@index.tensor".into()),
                        MonoType::Application {
                            c: "->".into(),
                            dimensions: None,
                            types: vec![
                                MonoType::Application {
                                    c: "ii".into(),
                                    dimensions: None,
                                    types: vec![],
                                },
                                MonoType::Application {
                                    c: format!("i{x}"),
                                    dimensions: None,
                                    types: vec![],
                                },
                            ],
                        },
                    ],
                })),
            }),
        );
    });

    vec![8, 16, 32, 64].into_iter().for_each(|x| {
        consumable_context.add_type_for_name(
            format!("@set.i{x}"),
            TIRType::PolyType(PolyType::TypeQuantifier {
                alpha: "@set.type".into(),
                sigma: Box::new(PolyType::MonoType(MonoType::Application {
                    c: "->".into(),
                    dimensions: None,
                    types: vec![
                        MonoType::Variable("@set.type".into()),
                        MonoType::Application {
                            c: "->".into(),
                            dimensions: None,
                            types: vec![
                                MonoType::Application {
                                    c: "ii".into(),
                                    dimensions: None,
                                    types: vec![],
                                },
                                MonoType::Application {
                                    c: "->".into(),
                                    dimensions: None,
                                    types: vec![
                                        MonoType::Application {
                                            c: format!("i{x}"),
                                            dimensions: None,
                                            types: vec![],
                                        },
                                        MonoType::Application {
                                            c: format!("i{x}"),
                                            dimensions: None,
                                            types: vec![],
                                        },
                                    ],
                                },
                            ],
                        },
                    ],
                })),
            }),
        );
    });

    consumable_context
}
