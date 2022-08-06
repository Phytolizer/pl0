use crate::data::Token;

#[derive(Debug)]
pub(crate) struct SyntaxTree {
    pub(crate) root: Box<Program>,
}

#[derive(Debug)]
pub(crate) struct Program {
    pub(crate) block: Block,
}

#[derive(Debug)]
pub(crate) struct Block {
    pub(crate) const_section: Option<Vec<ConstEntry>>,
    pub(crate) var_section: Option<VarSection>,
    pub(crate) procedures: Vec<Procedure>,
    pub(crate) body: Statement,
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
    pub(crate) name: Option<Token>,
    pub(crate) body: Option<Box<Block>>,
}

#[derive(Debug)]
pub(crate) struct Statement {
    pub(crate) inner: Option<StatementInner>,
}

#[derive(Debug)]
pub(crate) enum StatementInner {
    Assignment(AssignmentStatement),
    ProcedureCall(ProcedureCallStatement),
    Read(ReadStatement),
    Write(WriteStatement),
    Block(BlockStatement),
    If(Option<IfStatement>),
    While(WhileStatement),
}

#[derive(Debug)]
pub(crate) struct AssignmentStatement {
    pub(crate) variable: Token,
    pub(crate) expression: Option<Expression>,
}

#[derive(Debug)]
pub(crate) struct ProcedureCallStatement {
    pub(crate) name: Token,
}

#[derive(Debug)]
pub(crate) struct ReadStatement {
    pub(crate) name: Option<Token>,
}

#[derive(Debug)]
pub(crate) struct WriteStatement {
    pub(crate) expression: Option<Expression>,
}

#[derive(Debug)]
pub(crate) struct BlockStatement {
    pub(crate) statements: Vec<Statement>,
}

#[derive(Debug)]
pub(crate) struct IfStatement {
    pub(crate) condition: Condition,
    pub(crate) body: Box<Statement>,
}

#[derive(Debug)]
pub(crate) struct WhileStatement {
    pub(crate) condition: Condition,
    pub(crate) body: Box<Statement>,
}

#[derive(Debug)]
pub(crate) enum Condition {
    Odd(Option<OddCondition>),
    Comparison(ComparisonCondition),
}

#[derive(Debug)]
pub(crate) struct OddCondition {
    pub(crate) expression: Expression,
}

#[derive(Debug)]
pub(crate) struct ComparisonCondition {
    pub(crate) left: Expression,
    pub(crate) operator: Option<Token>,
    pub(crate) right: Option<Expression>,
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
    pub(crate) factor: Option<Factor>,
}

#[derive(Debug)]
pub(crate) enum Factor {
    Variable(Token),
    Number(Token),
    Parenthesized(Option<ParenthesizedFactor>),
}

#[derive(Debug)]
pub(crate) struct ParenthesizedFactor {
    pub(crate) expression: Box<Expression>,
}
