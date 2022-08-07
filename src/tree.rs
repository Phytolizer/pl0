use crate::data::Token;

#[derive(Debug)]
pub(crate) enum OrError<T> {
    Ok(T),
    Err,
}

impl<T> OrError<T> {
    pub(crate) fn map<U>(self, f: impl FnOnce(T) -> U) -> OrError<U> {
        match self {
            OrError::Ok(t) => OrError::Ok(f(t)),
            OrError::Err => OrError::Err,
        }
    }

    pub(crate) fn into_option(self) -> Option<T> {
        match self {
            OrError::Ok(t) => Some(t),
            OrError::Err => None,
        }
    }
}

impl<T> OrError<OrError<T>> {
    pub(crate) fn flatten(self) -> OrError<T> {
        match self {
            OrError::Ok(OrError::Ok(t)) => OrError::Ok(t),
            OrError::Ok(OrError::Err) => OrError::Err,
            OrError::Err => OrError::Err,
        }
    }
}

impl<T> FromIterator<OrError<T>> for OrError<Vec<T>> {
    fn from_iter<I: IntoIterator<Item = OrError<T>>>(iter: I) -> Self {
        let mut vec = Vec::new();
        for item in iter {
            match item {
                OrError::Ok(t) => vec.push(t),
                OrError::Err => return OrError::Err,
            }
        }
        OrError::Ok(vec)
    }
}

impl<T> From<Option<T>> for OrError<T> {
    fn from(opt: Option<T>) -> Self {
        match opt {
            Some(t) => OrError::Ok(t),
            None => OrError::Err,
        }
    }
}

#[derive(Debug)]
pub(crate) struct SyntaxTree {
    pub(crate) root: Box<Block>,
}

#[derive(Debug)]
pub(crate) struct Block {
    pub(crate) const_section: Option<Vec<ConstEntry>>,
    pub(crate) var_section: Option<VarSection>,
    pub(crate) procedures: Vec<OrError<Procedure>>,
    pub(crate) body: OrError<Statement>,
}

#[derive(Debug)]
pub(crate) struct ConstEntry {
    pub(crate) name: Token,
    pub(crate) equal_token: Option<Token>,
    pub(crate) value: Option<Token>,
}

#[derive(Debug)]
pub(crate) struct VarSection {
    pub(crate) var_kw: Token,
    pub(crate) entries: Option<Vec<Token>>,
    pub(crate) semicolon_token: Option<Token>,
}

#[derive(Debug)]
pub(crate) struct Procedure {
    pub(crate) name: Token,
    pub(crate) body: Box<Block>,
}

#[derive(Debug)]
pub(crate) enum Statement {
    Assignment(AssignmentStatement),
    ProcedureCall(Token),
    Read(Token),
    Write(Expression),
    Block(Vec<OrError<Statement>>),
    If(IfStatement),
    While(WhileStatement),
    Empty,
}

#[derive(Debug)]
pub(crate) struct AssignmentStatement {
    pub(crate) variable: Token,
    pub(crate) expression: Expression,
}

#[derive(Debug)]
pub(crate) struct IfStatement {
    pub(crate) condition: OrError<Condition>,
    pub(crate) body: OrError<Box<Statement>>,
}

#[derive(Debug)]
pub(crate) struct WhileStatement {
    pub(crate) condition: OrError<Condition>,
    pub(crate) body: OrError<Box<Statement>>,
}

#[derive(Debug)]
pub(crate) enum Condition {
    Odd(Expression),
    Comparison(ComparisonCondition),
}

#[derive(Debug)]
pub(crate) struct ComparisonCondition {
    pub(crate) left: Expression,
    pub(crate) operator: Token,
    pub(crate) right: Expression,
}

#[derive(Debug)]
pub(crate) struct Expression {
    pub(crate) sign: Option<Token>,
    pub(crate) first: Term,
    pub(crate) rest: Vec<PrefixedTerm>,
}

#[derive(Debug)]
pub(crate) struct PrefixedTerm {
    pub(crate) operator: Token,
    pub(crate) term: Term,
}

#[derive(Debug)]
pub(crate) struct Term {
    pub(crate) first: Factor,
    pub(crate) rest: Vec<PrefixedFactor>,
}

#[derive(Debug)]
pub(crate) struct PrefixedFactor {
    pub(crate) operator: Token,
    pub(crate) factor: Factor,
}

#[derive(Debug)]
pub(crate) enum Factor {
    Variable(Token),
    Number(Token),
    Parenthesized(OrError<Box<Expression>>),
}
