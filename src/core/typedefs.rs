
//===----------------------------------------------------------------------===//
///
/// typedefs.rs defines the 
/// compile time type information for the core library. This allows
/// the type system to check a program with that uses the core library 
///
//===----------------------------------------------------------------------===//


use crate::frontend::type_system::{
    context::Context,
    tir_types::{MonoType, PolyType, TIRType},
};

pub fn create_types_for_core() -> Context {
    let mut consumable_context = Context::new();

    consumable_context.add_type_for_name(
        "@print".into(),
        TIRType::PolyType(PolyType::TypeQuantifier {
            alpha: "@print.type".into(),
            sigma: Box::new(PolyType::MonoType(MonoType::Application {
                dimensions: None,
                c: "->".into(),
                types: vec![
                    MonoType::Variable("@print.type".into()),
                    MonoType::Variable("@print.type".into()),
                ],
            })),
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

    ["addi", "subi", "muli", "divi"].into_iter().for_each(|x| {
        consumable_context.add_type_for_name(
            format!("@{x}"),
            TIRType::PolyType(PolyType::TypeQuantifier {
                alpha: format!("@{x}.type"),
                sigma: Box::new(PolyType::MonoType(MonoType::Application {
                    c: "->".into(),
                    dimensions: None,
                    types: vec![
                        MonoType::Variable(format!("@{x}.type")),
                        MonoType::Application {
                            c: "->".into(),
                            dimensions: None,
                            types: vec![
                                MonoType::Variable(format!("@{x}.type")),
                                MonoType::Variable(format!("@{x}.type")),
                            ],
                        },
                    ],
                })),
            }),
        );
    });

    ["addf", "subf", "mulf", "divf"].into_iter().for_each(|x| {
        consumable_context.add_type_for_name(
            format!("@{x}"),
            TIRType::PolyType(PolyType::TypeQuantifier {
                alpha: format!("@{x}.type"),
                sigma: Box::new(PolyType::MonoType(MonoType::Application {
                    c: "->".into(),
                    dimensions: None,
                    types: vec![
                        MonoType::Variable(format!("@{x}.type")),
                        MonoType::Application {
                            c: "->".into(),
                            dimensions: None,
                            types: vec![
                                MonoType::Variable(format!("@{x}.type")),
                                MonoType::Variable(format!("@{x}.type")),
                            ],
                        },
                    ],
                })),
            }),
        );
    });

    ["ltei", "lti", "eqi", "grei", "gri"]
        .into_iter()
        .for_each(|x| {
            consumable_context.add_type_for_name(
                format!("@{x}"),
                TIRType::PolyType(PolyType::TypeQuantifier {
                    alpha: format!("@{x}.type"),
                    sigma: Box::new(PolyType::MonoType(MonoType::Application {
                        c: "->".into(),
                        dimensions: None,
                        types: vec![
                            MonoType::Variable(format!("@{x}.type")),
                            MonoType::Application {
                                c: "->".into(),
                                dimensions: None,
                                types: vec![
                                    MonoType::Variable(format!("@{x}.type")),
                                    MonoType::Application {
                                        c: "i1".into(),
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

    ["ltef", "ltf", "eqf", "gref", "grf"]
        .into_iter()
        .for_each(|x| {
            consumable_context.add_type_for_name(
                format!("@{x}"),
                TIRType::PolyType(PolyType::TypeQuantifier {
                    alpha: format!("@{x}.type"),
                    sigma: Box::new(PolyType::MonoType(MonoType::Application {
                        c: "->".into(),
                        dimensions: None,
                        types: vec![
                            MonoType::Variable(format!("@{x}.type")),
                            MonoType::Application {
                                c: "->".into(),
                                dimensions: None,
                                types: vec![
                                    MonoType::Variable(format!("@{x}.type")),
                                    MonoType::Application {
                                        c: "i1".into(),
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

    ["addi", "subi", "muli", "divi"].into_iter().for_each(|x| {
        consumable_context.add_type_for_name(
            format!("@{x}.v"),
            TIRType::PolyType(PolyType::TypeQuantifier {
                alpha: format!("@{x}.type"),
                sigma: Box::new(PolyType::MonoType(MonoType::Application {
                    c: "->".into(),
                    dimensions: None,
                    types: vec![
                        MonoType::Variable(format!("@{x}.type")),
                        MonoType::Application {
                            c: "->".into(),
                            dimensions: None,
                            types: vec![
                                MonoType::Variable(format!("@{x}.type")),
                                MonoType::Application {
                                    c: "->".into(),
                                    dimensions: None,
                                    types: vec![
                                        MonoType::Variable(format!("@{x}.type")),
                                        MonoType::Application {
                                            c: "ii".into(),
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

    ["addf", "subf", "mulf", "divf"].into_iter().for_each(|x| {
        consumable_context.add_type_for_name(
            format!("@{x}.v"),
            TIRType::PolyType(PolyType::TypeQuantifier {
                alpha: format!("@{x}.type"),
                sigma: Box::new(PolyType::MonoType(MonoType::Application {
                    c: "->".into(),
                    dimensions: None,
                    types: vec![
                        MonoType::Variable(format!("@{x}.type")),
                        MonoType::Application {
                            c: "->".into(),
                            dimensions: None,
                            types: vec![
                                MonoType::Variable(format!("@{x}.type")),
                                MonoType::Application {
                                    c: "->".into(),
                                    dimensions: None,
                                    types: vec![
                                        MonoType::Variable(format!("@{x}.type")),
                                        MonoType::Application {
                                            c: "ii".into(),
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

    vec![32, 64].into_iter().for_each(|x| {
        consumable_context.add_type_for_name(
            format!("@index.f{x}"),
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
                                    c: format!("f{x}"),
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

    consumable_context.add_type_for_name(
        format!("@empty"),
        TIRType::PolyType(PolyType::TypeQuantifier {
            alpha: "@empty.type".into(),
            sigma: Box::new(PolyType::TypeQuantifier {
                alpha: "@empty.result".into(),
                sigma: Box::new(PolyType::MonoType(MonoType::Application {
                    c: "->".into(),
                    dimensions: None,
                    types: vec![
                        MonoType::Variable("@empty.type".into()),
                        MonoType::Application {
                            c: "->".into(),
                            dimensions: None,
                            types: vec![
                                MonoType::Application {
                                    c: "ii".into(),
                                    dimensions: None,
                                    types: vec![],
                                },
                                MonoType::Variable("@empty.result".into()),
                            ],
                        },
                    ],
                })),
            }),
        }),
    );

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

    vec![32, 64].into_iter().for_each(|x| {
        consumable_context.add_type_for_name(
            format!("@set.f{x}"),
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
                                            c: format!("f{x}"),
                                            dimensions: None,
                                            types: vec![],
                                        },
                                        MonoType::Application {
                                            c: format!("f{x}"),
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

    ["read", "write"].into_iter().for_each(|x| {
        consumable_context.add_type_for_name(
            format!("@prefetch.{x}"),
            TIRType::PolyType(PolyType::TypeQuantifier {
                alpha: "@prefetch.type".into(),
                sigma: Box::new(PolyType::MonoType(MonoType::Application {
                    c: "->".into(),
                    dimensions: None,
                    types: vec![
                        MonoType::Variable(format!("@prefetch.{x}.type")),
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
                                    c: "ii".into(),
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

    consumable_context.add_type_for_name(
        format!("@induction_variable.unroll"),
        TIRType::PolyType(PolyType::TypeQuantifier {
            alpha: "@induction_variable.unroll.type".into(),
            sigma: Box::new(PolyType::MonoType(MonoType::Application {
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
                                c: "ii".into(),
                                dimensions: None,
                                types: vec![],
                            },
                            MonoType::Variable(format!("@induction_variable.unroll.type")),
                        ],
                    },
                ],
            })),
        }),
    );

    consumable_context.add_type_for_name(
        format!("@induction_variable.vector.get"),
        TIRType::PolyType(PolyType::TypeQuantifier {
            alpha: "@induction_variable.vector.get.type".into(),
            sigma: Box::new(PolyType::MonoType(MonoType::Application {
                c: "->".into(),
                dimensions: None,
                types: vec![
                    MonoType::Variable(format!("@induction_variable.unroll.type")),
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
                                c: "ii".into(),
                                dimensions: None,
                                types: vec![],
                            },
                        ],
                    },
                ],
            })),
        }),
    );

    consumable_context.add_type_for_name(
        format!("@vec.load"),
        TIRType::PolyType(PolyType::TypeQuantifier {
            alpha: "@vec.type".into(),
            sigma: Box::new(PolyType::MonoType(MonoType::Application {
                c: "->".into(),
                dimensions: None,
                types: vec![
                    MonoType::Variable("@vec.type".into()),
                    MonoType::Application {
                        c: "->".into(),
                        dimensions: None,
                        types: vec![
                            MonoType::Application {
                                c: "ii".into(), //offset
                                dimensions: None,
                                types: vec![],
                            },
                            MonoType::Application {
                                c: "->".into(),
                                dimensions: None,
                                types: vec![
                                    MonoType::Application {
                                        c: "ii".into(), //size
                                        dimensions: None,
                                        types: vec![],
                                    },
                                    MonoType::Variable("@vec.type".into()),
                                ],
                            },
                        ],
                    },
                ],
            })),
        }),
    );

    consumable_context.add_type_for_name(
        format!("@vec.store"),
        TIRType::PolyType(PolyType::TypeQuantifier {
            alpha: "@vec.in".into(),
            sigma: Box::new(PolyType::TypeQuantifier {
                alpha: "@vec.out".into(),
                sigma: Box::new(PolyType::MonoType(MonoType::Application {
                    c: "->".into(),
                    dimensions: None,
                    types: vec![
                        MonoType::Variable("@vec.in".into()),
                        MonoType::Application {
                            c: "->".into(),
                            dimensions: None,
                            types: vec![
                                MonoType::Variable("@vec.out".into()),
                                MonoType::Application {
                                    c: "->".into(),
                                    dimensions: None,
                                    types: vec![
                                        MonoType::Application {
                                            c: "ii".into(), //size
                                            dimensions: None,
                                            types: vec![],
                                        },
                                        MonoType::Application {
                                            c: "ii".into(), //size
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
        }),
    );

    consumable_context
}
