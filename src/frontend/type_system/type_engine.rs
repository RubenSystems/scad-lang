// given an HIR expression, return a type

// define TIR(tm) (typed intermediate repr)

use std::{
    clone,
    collections::{HashMap, HashSet},
    string,
};

static mut CURRENT_TMP_INDEX: u128 = 0;

pub fn generate_type_name() -> String {
    let val = unsafe { CURRENT_TMP_INDEX };
    unsafe { CURRENT_TMP_INDEX += 1 };
    format!("t{val}")
}

trait FreeVarsGettable {
    fn free_vars(&self) -> Vec<String>;
}

#[derive(Debug)]
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

    pub fn merge(&self, other: &Substitution) -> Self {
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

    fn substitute_mono(&self, tpe: &MonoType) -> MonoType {
        match tpe {
            MonoType::Variable(v) => match self.from_to.get(v) {
                Some(s) => s.clone(),
                None => MonoType::Variable(v.clone()),
            },
            MonoType::Application { C, types } => MonoType::Application {
                C: C.clone(),
                types: types.iter().map(|x| self.substitute_mono(x)).collect(),
            },
        }
    }

    fn substitute_poly(&self, tpe: &PolyType) -> PolyType {
        match tpe {
            PolyType::MonoType(m) => PolyType::MonoType(self.substitute_mono(m)),
            PolyType::TypeQuantifier { alpha, sigma } => PolyType::TypeQuantifier {
                alpha: alpha.clone(),
                sigma: Box::new(self.substitute_poly(&sigma)),
            },
        }
    }
}

#[derive(Debug, Clone)]
pub enum MonoType {
    Variable(String),
    Application { C: String, types: Vec<MonoType> },
}

#[derive(Debug, Clone)]
pub enum PolyType {
    MonoType(MonoType),
    TypeQuantifier { alpha: String, sigma: Box<PolyType> },
}

#[derive(Debug, Clone)]
pub enum TIRType {
    MonoType(MonoType),
    PolyType(PolyType),
}

pub enum TIRExpression {
    VariableReference {
        name: String,
    },
    VariableDecl {
        name: String,
        e1: Box<TIRExpression>,
        e2: Box<TIRExpression>,
    },
    FunctionCall {
        e1: Box<TIRExpression>,
        e2: Box<TIRExpression>,
    },
    FunctionDefinition {
        arg_name: String,
        e1: Box<TIRExpression>,
    },
}

#[derive(Clone)]
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

    pub fn applying_substitution(&self, sub: &Substitution) -> Context {
        Context {
            env: self
                .env
                .iter()
                .map(|(k, v)| (k.clone(), sub.substitute(v)))
                .collect(),
        }
    }
}

pub fn w_algo(context: &Context, exp: &TIRExpression) -> (Substitution, MonoType) {
    match exp {
        TIRExpression::VariableReference { name } => {
            let Some(tpe) = context.get_type_for_name(name) else {
                unreachable!("Undefined variable reference!")
            };
            let TIRType::MonoType(m) = tpe else {
                unreachable!("Can only return monotypes")
            };
            (Substitution::new(), m.clone())
        }
        TIRExpression::VariableDecl { name, e1, e2 } => {
            let (s1, t1) = w_algo(context, &e1);

            let mut sub_context = context.clone();
            sub_context.add_type_for_name(
                name.clone(),
                s1.substitute(&TIRType::PolyType(generalise(context, t1))),
            );
            let (s2, t2) = w_algo(&sub_context, &e2);

            (s2.merge(&s1), t2)
        }
        TIRExpression::FunctionCall { e1, e2 } => {
            let (s1, t1) = w_algo(context, e1);
            let (s2, t2) = w_algo(&context.applying_substitution(&s1), e2);
            let b = generate_type_name();

            let s3 = unify(
                &s1.substitute_mono(&t1),
                &MonoType::Application {
                    C: "->".into(),
                    types: vec![t2, MonoType::Variable(b.clone())],
                },
            );

            (
                s3.merge(&s2.merge(&s1)),
                s3.substitute_mono(&MonoType::Variable(b)),
            )
        }
        TIRExpression::FunctionDefinition { arg_name: name, e1 } => {
            let new_type = generate_type_name();

            let mut new_context = context.clone();
            new_context.add_type_for_name(
                name.into(),
                TIRType::MonoType(MonoType::Variable(new_type.clone())),
            );
            let (sub, tpe) = w_algo(&new_context, e1);

            (
                sub,
                MonoType::Application {
                    C: "->".into(),
                    types: vec![MonoType::Variable(new_type), tpe],
                },
            )
        }
    }
}

fn generalise(ctx: &Context, tpe: MonoType) -> PolyType {
    let free_variables = diff(ctx.free_vars(), tpe.free_vars());
    let mut base_polytype = PolyType::MonoType(tpe);
    free_variables.iter().for_each(|fv| {
        base_polytype = PolyType::TypeQuantifier {
            alpha: fv.clone(),
            sigma: Box::new(base_polytype.clone()),
        }
    });

    base_polytype
}

fn contains(v: &MonoType, tpe: &String) -> bool {
    match v {
        MonoType::Variable(v) => *v == *tpe,
        MonoType::Application { C: _, types } => {
            let inner_contains: Vec<bool> = types
                .iter()
                .map(|x| contains(x, tpe))
                .filter(|x| *x)
                .collect();
            !inner_contains.is_empty()
        }
    }
}

fn unify(t1: &MonoType, t2: &MonoType) -> Substitution {
    if let (MonoType::Variable(a), MonoType::Variable(b)) = (&t1, &t2) {
        if a == b {
            return Substitution::new();
        }
    };
    if let MonoType::Variable(a) = &t1 {
        if contains(&t2, a) {
            unreachable!("infiniteee waaaaaaa")
        };
        let mut new_sub = Substitution::new();
        new_sub.add_sub(a.clone(), t2.clone());
        return new_sub;
    }

    if let MonoType::Variable(_) = &t2 {
        return unify(t2, t1);
    };

    if let (
        MonoType::Application { C: c1, types: t1 },
        MonoType::Application { C: c2, types: t2 },
    ) = (t1, t2)
    {
        if c1 != c2 {
            unreachable!("Can't unify different functions")
        }
        if t1.len() != t2.len() {
            unreachable!("Can't unify functions with different numbers of args")
        }

        let mut s = Substitution::new();
        t1.iter()
            .zip(t2.iter())
            .for_each(|(a, b)| s = s.merge(&unify(a, b)));
        return s;
    }

    todo!()
}

fn diff(a: Vec<String>, b: Vec<String>) -> Vec<String> {
    let vars: HashSet<_> = a.into_iter().collect();
    b.into_iter().filter(|x| !vars.contains(x)).collect()
}

impl FreeVarsGettable for TIRType {
    fn free_vars(&self) -> Vec<String> {
        match self {
            TIRType::MonoType(m) => m.free_vars(),
            TIRType::PolyType(p) => p.free_vars(),
        }
    }
}

impl FreeVarsGettable for Context {
    fn free_vars(&self) -> Vec<String> {
        self.env.iter().flat_map(|(_, v)| v.free_vars()).collect()
    }
}

impl FreeVarsGettable for MonoType {
    fn free_vars(&self) -> Vec<String> {
        match self {
            MonoType::Variable(v) => vec![v.to_string()],
            MonoType::Application { C: _, types } => {
                types.iter().flat_map(|x| x.free_vars()).collect()
            }
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

/*
    unify(a: MonoType, b: MonoType) → Substitution:
        if a is a type variable:
        if bis the same type variable:
        return {}
        if b contains a:
        throw error "occurs check failed, cannot create infinite type"
        return { a → b } if b is a type variable:
        return unify(b, a)
        if a and b are both type function applications:
        if a and b have different type functions:
        throw error "failed to unify, different type functions"
        let S = 13
        for i in range (number of type function arguments):
        S = combine(S, unify(S(a.arguments[il), S(b.arguments[il)))
        return S

*/

/*
use std::collections::HashMap;
use crate::frontend::high_level_ir::ast_types::{VariableDecl, Expression, Statement};

pub struct Context {
    pub variables: HashMap<String, Types>,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Types {
    Unknown,
    Integer,
    Float,
}


fn type_check_program(program: Vec<Statement>, upper_context: &Context, layer: i32) {

}

fn type_check_statement(statement: Statement, upper_context: &Context, layer_context: &mut Context, layer: i32) -> bool {

    match statement {
        Statement::ConstDecl(c) => {

        },
        Statement::VariableDecl(_) => todo!(),
        Statement::Expression(_) => todo!(),
        Statement::ConditionalStatementControlFlow { if_blocks, else_block } => todo!(),
        Statement::FunctionDefinition(f) => todo!(),
        Statement::ProcedureDefinition(_) => todo!(),
        Statement::Block(_) => todo!(),
    }
}

impl Context {

    pub fn new () -> Self {
        Self {
            variables: HashMap::<String, Types>::new(),
        }
    }

    pub fn merge(&self, other: &Context) -> Self {
        let mut new_ctx = self.variables.clone();
        new_ctx.extend(other.variables.clone());
        Self {
            variables: new_ctx
        }
    }

    pub fn set_type_for_variable(&mut self, variable_name: String, type_name: String) {
        self.variables.insert(variable_name, Types::Integer);
    }

    pub fn type_check_expression(&self, expression: &Expression) -> Option<Types> {

        match expression {
            Expression::InfixOperation(_) => todo!(),// Going to be removed later
            Expression::Float(_) => Some(Types::Float),
            Expression::Integer(_) => Some(Types::Integer),
            Expression::CharArray(_) => todo!(),
            Expression::Identifier(id) => {
                match self.variables.get(&id.0) {
                    Some(t) => Some(t.clone()),
                    _ => None
                }
            },
            Expression::ConditionalExpressionControlFlowControl { if_blocks, else_block } => {
                let mut types: Vec<Option<Types>> = if_blocks.iter().map(|i| {
                    self.type_check_expression(&i.block.expression)
                }).collect();

                types.push(self.type_check_expression(&else_block.expression));

                // make sure all types are equal otherwise thats just wrong
                if !types.is_empty() && types.windows(2).all(|w| w[0] == w[1]) {
                    types[0].clone()
                } else {
                    unreachable!("Expression if statements must all be of the same type dumbo")
                }
            },
            Expression::FunctionCall(f) => todo!(),
            Expression::Block(b) => self.type_check_expression(&b.expression),
        }

    }
}

*/
