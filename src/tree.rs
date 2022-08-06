use crate::data::Token;

#[derive(Debug)]
pub(crate) struct SyntaxTree {
    pub(crate) root: Box<Program>,
}

#[derive(Debug)]
pub(crate) struct Program {
    pub(crate) block: Block,
    pub(crate) period_token: Token,
}

#[derive(Debug)]
pub(crate) struct Block {
    pub(crate) const_section: Option<ConstSection>,
    pub(crate) var_section: Option<VarSection>,
    pub(crate) procedures: Vec<Procedure>,
    pub(crate) body: Statement,
}

#[derive(Debug)]
pub(crate) struct ConstSection {
    pub(crate) const_kw: Token,
    pub(crate) entries: ConstEntries,
    pub(crate) semicolon_token: Token,
}

#[derive(Debug)]
pub(crate) struct ConstEntries {
    pub(crate) first: ConstEntry,
    pub(crate) rest: Vec<PrefixedConstEntry>,
}

#[derive(Debug)]
pub(crate) struct PrefixedConstEntry {
    pub(crate) comma_token: Token,
    pub(crate) entry: ConstEntry,
}

#[derive(Debug)]
pub(crate) struct ConstEntry {
    pub(crate) name: Token,
    pub(crate) equal_token: Token,
    pub(crate) value: Token,
}

#[derive(Debug)]
pub(crate) struct VarSection {
    pub(crate) var_kw: Token,
    pub(crate) entries: VarEntries,
    pub(crate) semicolon_token: Token,
}

#[derive(Debug)]
pub(crate) struct VarEntries {
    pub(crate) first: VarEntry,
    pub(crate) rest: Vec<PrefixedVarEntry>,
}

#[derive(Debug)]
pub(crate) struct PrefixedVarEntry {
    pub(crate) comma_token: Token,
    pub(crate) entry: VarEntry,
}

#[derive(Debug)]
pub(crate) struct VarEntry {
    pub(crate) name: Token,
}

#[derive(Debug)]
pub(crate) struct Procedure {
    pub(crate) procedure_kw: Token,
    pub(crate) name: Token,
    pub(crate) first_semicolon_token: Token,
    pub(crate) block: Box<Block>,
    pub(crate) second_semicolon_token: Token,
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
    If(IfStatement),
    While(WhileStatement),
}

#[derive(Debug)]
pub(crate) struct PrefixedStatement {
    pub(crate) semicolon_token: Token,
    pub(crate) statement: Statement,
}

#[derive(Debug)]
pub(crate) struct AssignmentStatement {
    pub(crate) variable: Token,
    pub(crate) colon_equal_token: Token,
    pub(crate) expression: Expression,
}

#[derive(Debug)]
pub(crate) struct ProcedureCallStatement {
    pub(crate) call_kw: Token,
    pub(crate) name: Token,
}

#[derive(Debug)]
pub(crate) struct ReadStatement {
    pub(crate) question_mark_token: Token,
    pub(crate) name: Token,
}

#[derive(Debug)]
pub(crate) struct WriteStatement {
    pub(crate) exclamation_mark_token: Token,
    pub(crate) expression: Expression,
}

#[derive(Debug)]
pub(crate) struct BlockStatement {
    pub(crate) begin_kw: Token,
    pub(crate) first: Box<Statement>,
    pub(crate) rest: Vec<PrefixedStatement>,
    pub(crate) end_kw: Token,
}

#[derive(Debug)]
pub(crate) struct IfStatement {
    pub(crate) if_kw: Token,
    pub(crate) condition: Condition,
    pub(crate) then_kw: Token,
    pub(crate) body: Box<Statement>,
}

#[derive(Debug)]
pub(crate) struct WhileStatement {
    pub(crate) while_kw: Token,
    pub(crate) condition: Condition,
    pub(crate) do_kw: Token,
    pub(crate) body: Box<Statement>,
}

#[derive(Debug)]
pub(crate) enum Condition {
    Odd(OddCondition),
    Comparison(ComparisonCondition),
}

#[derive(Debug)]
pub(crate) struct OddCondition {
    pub(crate) odd_kw: Token,
    pub(crate) expression: Expression,
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
    Parenthesized(ParenthesizedFactor),
}

#[derive(Debug)]
pub(crate) struct ParenthesizedFactor {
    pub(crate) left_parenthesis_token: Token,
    pub(crate) expression: Box<Expression>,
    pub(crate) right_parenthesis_token: Token,
}
