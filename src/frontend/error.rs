use std::{
    collections::HashMap,
    fmt::{self, Display, Formatter},
};

use pest::error::Error;

use super::high_level_ir::hir_parser::Rule;

pub type PoolID = usize;
pub struct ErrorPool {
    counter: usize,
    pool: HashMap<PoolID, ErrorLocation>,
}

impl ErrorPool {
    pub fn new() -> Self {
        Self {
            counter: 0,
            pool: HashMap::new(),
        }
    }

    pub fn insert(&mut self, pair: &pest::iterators::Pair<'_, Rule>) -> PoolID {
        self.pool
            .insert(self.counter, ErrorLocation::from_pair(pair));
        let ccounter = self.counter;
        self.counter += 1;
        ccounter
    }

	pub fn get(&self, pid: PoolID) -> &ErrorLocation {
		self.pool.get(&pid).unwrap()
	}
}

#[derive(Debug)]
pub enum ErrorType {
    ParsingError,
	InvalidType,
	InvalidInput,
	UndefinedVariableReference,
	UnsupportedVariableReassignment,
	UnableToTypeFunctionArguement,
	CannotCheckReturnType,
	CannotTypeExpression,
	CannotTypeCheckConditionalCondition,
	ConditionalDoesNotHaveBooleanCondition,
	MultipleBranchTypesInConditional,
	MultipleTypesInVector

}

#[derive(Debug, Clone)]
pub struct ErrorLocation {
    user: String,
    line: usize,
    column: usize,
}
impl ErrorLocation {
    pub fn new(user: &str, line: usize, column: usize) -> Self {
        Self {
            user: user.trim().to_string(),
            line,
            column,
        }
    }

    pub fn from_pair(pair: &pest::iterators::Pair<'_, Rule>) -> Self {
        let (line, column) = pair.line_col();
        let user = pair.as_str();

        Self::new(user, line, column)
    }
}

#[derive(Debug)]
pub struct SCADError {
    location_info: ErrorLocation,
    error_type: ErrorType,
}

impl Display for SCADError {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        // Define the formatting you want here

        let _ = match self.error_type {
            ErrorType::ParsingError => write!(f, "\n| Cannot parse input"),
            ErrorType::InvalidType => write!(f, "\n| Invalid input type"),
            ErrorType::InvalidInput => write!(f, "\n| Invalid input "),
            ErrorType::UndefinedVariableReference => write!(f, "\n| Undefined variable reference "),
            ErrorType::UnsupportedVariableReassignment => write!(f, "\n| You have attempted to redefine a varaible that already exists "),
            ErrorType::UnableToTypeFunctionArguement => write!(f, "\n| Cannot type check function arguement"),
            ErrorType::CannotCheckReturnType => write!(f, "\n| Cannot type check return type of function"),
            ErrorType::CannotTypeExpression => write!(f, "\n| Cannot type check this expression"),
            ErrorType::CannotTypeCheckConditionalCondition => write!(f, "\n| Cannot type check the condition of this branch"),
            ErrorType::ConditionalDoesNotHaveBooleanCondition => write!(f, "\n| Condition is not of a boolean type"),
            ErrorType::MultipleBranchTypesInConditional => write!(f, "\n| Branches have divergent types"),
            ErrorType::MultipleTypesInVector => write!(f, "\n| Multiple types in this vector"),
        };
        _ = write!(
            f,
            "\n| in {}:{} -->",
            self.location_info.line, self.location_info.column
        );
        _ = write!(f, "\n\n{}\n\n", self.location_info.user);

        Ok(())
    }
}

impl SCADError {

    pub fn from_parse_error(parse_eror: Error<Rule>) -> Self {
        let (line, column) = match parse_eror.line_col {
            pest::error::LineColLocation::Pos(p) => p,
            pest::error::LineColLocation::Span(l, _) => l,
        };

        Self {
            location_info: ErrorLocation::new(parse_eror.line(), line, column),
            error_type: ErrorType::ParsingError,
        }
    }

	pub fn from_pair(error_type: ErrorType, pair: &pest::iterators::Pair<'_, Rule>) -> Self {
        let location_info = ErrorLocation::from_pair(pair);

        Self {
            location_info,
            error_type,
        }
    }

	pub fn from_pid(error_type: ErrorType, pid: PoolID, pool: &ErrorPool) -> Self {
        let location_info = pool.get(pid).clone();

        Self {
            location_info,
            error_type,
        }
    }
}
