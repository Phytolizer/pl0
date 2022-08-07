use std::cell::RefCell;
use std::fmt::Display;
use std::iter::Enumerate;
use std::num::NonZeroUsize;
use std::ops::Range;
use std::ops::RangeFrom;
use std::ops::RangeFull;
use std::ops::RangeTo;

use nom::branch::alt;
use nom::bytes::complete::take;
use nom::combinator::all_consuming;
use nom::combinator::map;
use nom::combinator::opt;
use nom::combinator::verify;
use nom::multi::many0;
use nom::sequence::pair;
use nom::sequence::preceded;
use nom::sequence::terminated;
use nom::sequence::tuple;
use nom::IResult;
use nom::InputIter;
use nom::InputLength;
use nom::InputTake;
use nom::Needed;
use nom::Parser;
use nom::Slice;

use crate::data::SourceLocation;
use crate::data::Token;
use crate::data::TokenType;
use crate::tree::AssignmentStatement;
use crate::tree::Block;
use crate::tree::ComparisonCondition;
use crate::tree::Condition;
use crate::tree::ConstEntry;
use crate::tree::Expression;
use crate::tree::Factor;
use crate::tree::IfStatement;
use crate::tree::OrError;
use crate::tree::PrefixedFactor;
use crate::tree::PrefixedTerm;
use crate::tree::Procedure;
use crate::tree::Statement;
use crate::tree::SyntaxTree;
use crate::tree::Term;
use crate::tree::VarSection;
use crate::tree::WhileStatement;

#[derive(Debug)]
pub(crate) struct Error {
    pub(crate) location: Option<SourceLocation>,
    pub(crate) message: String,
}

impl Error {
    fn new(location: Option<SourceLocation>, message: String) -> Self {
        Self { location, message }
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}: {}",
            self.location
                .as_ref()
                .map(|l| l.to_string())
                .unwrap_or_else(|| String::from("end of input")),
            self.message
        )
    }
}

#[derive(Debug, Clone)]
struct State<'a>(&'a RefCell<Vec<Error>>);

impl<'a> State<'a> {
    fn report_error(&self, error: Error) {
        self.0.borrow_mut().push(error);
    }
}

#[derive(Debug, Clone, Copy)]
struct TokenInput<'a> {
    tokens: &'a [Token],
    state: &'a State<'a>,
    start: usize,
    end: usize,
}

impl<'a> TokenInput<'a> {
    fn new(tokens: &'a [Token], state: &'a State<'a>) -> Self {
        Self {
            tokens,
            state,
            start: 0,
            end: tokens.len(),
        }
    }
}

impl<'a> InputLength for TokenInput<'a> {
    fn input_len(&self) -> usize {
        self.tokens.len()
    }
}

impl<'a> InputTake for TokenInput<'a> {
    fn take(&self, count: usize) -> Self {
        Self {
            tokens: &self.tokens[self.start..self.start + count],
            state: self.state,
            start: 0,
            end: count,
        }
    }

    fn take_split(&self, count: usize) -> (Self, Self) {
        let (prefix, suffix) = self.tokens.split_at(count);
        (Self::new(suffix, self.state), Self::new(prefix, self.state))
    }
}

impl<'a> Slice<Range<usize>> for TokenInput<'a> {
    fn slice(&self, range: Range<usize>) -> Self {
        Self {
            tokens: self.tokens.slice(range.clone()),
            state: self.state,
            start: self.start + range.start,
            end: self.start + range.end,
        }
    }
}

impl<'a> Slice<RangeTo<usize>> for TokenInput<'a> {
    fn slice(&self, range: RangeTo<usize>) -> Self {
        self.slice(0..range.end)
    }
}

impl<'a> Slice<RangeFrom<usize>> for TokenInput<'a> {
    fn slice(&self, range: RangeFrom<usize>) -> Self {
        self.slice(range.start..self.end - self.start)
    }
}

impl<'a> Slice<RangeFull> for TokenInput<'a> {
    fn slice(&self, _: RangeFull) -> Self {
        Self {
            tokens: self.tokens,
            state: self.state,
            start: self.start,
            end: self.end,
        }
    }
}

impl<'a> InputIter for TokenInput<'a> {
    type Item = &'a Token;
    type IterElem = std::slice::Iter<'a, Token>;
    type Iter = Enumerate<Self::IterElem>;

    fn iter_indices(&self) -> Self::Iter {
        self.tokens.iter().enumerate()
    }

    fn iter_elements(&self) -> Self::IterElem {
        self.tokens.iter()
    }

    fn position<P>(&self, predicate: P) -> Option<usize>
    where
        P: Fn(Self::Item) -> bool,
    {
        self.tokens.iter().position(predicate)
    }

    fn slice_index(&self, count: usize) -> Result<usize, nom::Needed> {
        if self.tokens.len() >= count {
            Ok(count)
        } else {
            Err(Needed::Size(unsafe {
                NonZeroUsize::new_unchecked(count - self.tokens.len())
            }))
        }
    }
}

fn expect<'a, F, E, T>(
    mut parser: F,
    error_msg: E,
) -> impl FnMut(TokenInput<'a>) -> IResult<TokenInput<'a>, Option<T>>
where
    F: FnMut(TokenInput<'a>) -> IResult<TokenInput<'a>, T>,
    E: ToString,
{
    move |input| match parser(input) {
        Ok((remaining, out)) => Ok((remaining, Some(out))),
        Err(nom::Err::Error(e) | nom::Err::Failure(e)) => {
            let err = Error::new(
                (!e.input.tokens.is_empty()).then(|| e.input.tokens[0].source_location.clone()),
                error_msg.to_string(),
            );
            e.input.state.report_error(err);
            Ok((e.input, None))
        }
        Err(e) => Err(e),
    }
}

fn take_only_token(t: TokenInput) -> Token {
    t.tokens[0].clone()
}

fn token<'a>(ty: TokenType) -> impl FnMut(TokenInput<'a>) -> IResult<TokenInput<'a>, Token> {
    map(
        verify(take(1usize), move |t: &TokenInput| {
            t.tokens[0].token_type == ty
        }),
        take_only_token,
    )
}

fn ident<'a>() -> impl FnMut(TokenInput<'a>) -> IResult<TokenInput<'a>, Token> {
    map(
        verify(take(1usize), move |t: &TokenInput| {
            matches!(t.tokens[0].token_type, TokenType::Ident(_))
        }),
        take_only_token,
    )
}

fn number<'a>() -> impl FnMut(TokenInput<'a>) -> IResult<TokenInput<'a>, Token> {
    map(
        verify(take(1usize), move |t: &TokenInput| {
            matches!(t.tokens[0].token_type, TokenType::Number(_))
        }),
        take_only_token,
    )
}

fn const_entry<'a>() -> impl FnMut(TokenInput<'a>) -> IResult<TokenInput<'a>, ConstEntry> {
    map(
        tuple((
            ident(),
            expect(token(TokenType::Equal), "Expected '=' after constant name"),
            expect(number(), "Expected number after '='"),
        )),
        |(name, equal_token, value)| ConstEntry {
            name,
            equal_token,
            value,
        },
    )
}

fn prefixed_const_entry<'a>(
) -> impl FnMut(TokenInput<'a>) -> IResult<TokenInput<'a>, Option<ConstEntry>> {
    preceded(
        token(TokenType::Comma),
        expect(const_entry(), "Expected another entry after ','"),
    )
}

fn const_entries<'a>() -> impl FnMut(TokenInput<'a>) -> IResult<TokenInput<'a>, Vec<ConstEntry>> {
    map(
        pair(const_entry(), many0(prefixed_const_entry())),
        |(first, rest)| {
            let mut entries = vec![first];
            entries.extend(rest.into_iter().filter_map(|e| e));
            entries
        },
    )
}

fn const_section<'a>() -> impl FnMut(TokenInput<'a>) -> IResult<TokenInput<'a>, Vec<ConstEntry>> {
    map(
        tuple((
            token(TokenType::KwConst),
            expect(const_entries(), "Expected constant name after 'const'"),
            expect(token(TokenType::Semicolon), "Expected ';' after constants"),
        )),
        |(_, entries, _)| entries.unwrap_or(Vec::new()),
    )
}

fn prefixed_var_entry<'a>() -> impl FnMut(TokenInput<'a>) -> IResult<TokenInput<'a>, Token> {
    preceded(token(TokenType::Comma), ident())
}

fn var_entries<'a>() -> impl FnMut(TokenInput<'a>) -> IResult<TokenInput<'a>, Vec<Token>> {
    map(
        pair(ident(), many0(prefixed_var_entry())),
        |(first, rest)| {
            let mut entries = vec![first];
            entries.extend(rest.into_iter());
            entries
        },
    )
}

fn var_section<'a>() -> impl FnMut(TokenInput<'a>) -> IResult<TokenInput<'a>, VarSection> {
    map(
        tuple((
            token(TokenType::KwVar),
            expect(var_entries(), "Expected variable name after 'var'"),
            expect(token(TokenType::Semicolon), "Expected ';' after variables"),
        )),
        |(var_kw, entries, semicolon_token)| VarSection {
            var_kw,
            entries,
            semicolon_token,
        },
    )
}

fn procedure(input: TokenInput) -> IResult<TokenInput, Option<Procedure>> {
    map(
        tuple((
            token(TokenType::KwProcedure),
            expect(ident(), "Expected procedure name"),
            expect(
                token(TokenType::Semicolon),
                "Expected ';' after procedure name",
            ),
            expect(map(block(), Box::new), "Expected procedure body after ';'"),
            expect(
                token(TokenType::Semicolon),
                "Expected ';' after procedure body",
            ),
        )),
        |(_, name, _, body, _)| {
            Some(Procedure {
                name: name?,
                body: body?,
            })
        },
    )(input)
}

fn prefixed_factor<'a>(
) -> impl FnMut(TokenInput<'a>) -> IResult<TokenInput<'a>, Option<PrefixedFactor>> {
    map(
        pair(
            alt((token(TokenType::Asterisk), token(TokenType::Slash))),
            expect(factor(), "Expected factor after '*' or '/'"),
        ),
        |(operator, factor)| {
            Some(PrefixedFactor {
                operator,
                factor: factor?,
            })
        },
    )
}

fn parenthesized_factor(input: TokenInput) -> IResult<TokenInput, Option<Box<Expression>>> {
    map(
        tuple((
            token(TokenType::LeftParen),
            expect(map(expression(), Box::new), "Expected expression after '('"),
            expect(
                token(TokenType::RightParen),
                "Expected ')' after expression",
            ),
        )),
        |(_, expression, _)| expression.map(|e| *e).flatten().map(Box::new),
    )(input)
}

fn factor<'a>() -> impl FnMut(TokenInput<'a>) -> IResult<TokenInput<'a>, Factor> {
    alt((
        map(ident(), Factor::Variable),
        map(number(), Factor::Number),
        map(parenthesized_factor, |f| Factor::Parenthesized(f.into())),
    ))
}

fn prefixed_term<'a>() -> impl FnMut(TokenInput<'a>) -> IResult<TokenInput<'a>, Option<PrefixedTerm>>
{
    map(
        pair(
            alt((token(TokenType::Plus), token(TokenType::Minus))),
            term(),
        ),
        |(operator, term)| {
            Some(PrefixedTerm {
                operator,
                term: term?,
            })
        },
    )
}

fn term<'a>() -> impl FnMut(TokenInput<'a>) -> IResult<TokenInput<'a>, Option<Term>> {
    map(pair(factor(), many0(prefixed_factor())), |(first, rest)| {
        Some(Term {
            first,
            rest: rest.into_iter().collect::<Option<Vec<_>>>()?,
        })
    })
}

fn expression<'a>() -> impl FnMut(TokenInput<'a>) -> IResult<TokenInput<'a>, Option<Expression>> {
    map(
        tuple((
            opt(alt((token(TokenType::Plus), token(TokenType::Minus)))),
            term(),
            many0(prefixed_term()),
        )),
        |(sign, first, rest)| {
            Some(Expression {
                sign,
                first: first?,
                rest: rest.into_iter().collect::<Option<Vec<_>>>()?,
            })
        },
    )
}

fn odd_condition<'a>() -> impl FnMut(TokenInput<'a>) -> IResult<TokenInput<'a>, OrError<Expression>>
{
    map(
        preceded(
            token(TokenType::KwOdd),
            expect(expression(), "Expected expression after 'odd'"),
        ),
        |e| e.flatten().into(),
    )
}

fn comparison_condition<'a>(
) -> impl FnMut(TokenInput<'a>) -> IResult<TokenInput<'a>, Option<ComparisonCondition>> {
    map(
        tuple((
            expression(),
            expect(
                alt((
                    token(TokenType::Equal),
                    token(TokenType::Hash),
                    token(TokenType::LessThan),
                    token(TokenType::LessThanEqual),
                    token(TokenType::GreaterThan),
                    token(TokenType::GreaterThanEqual),
                )),
                "Expected comparison operator after expression",
            ),
            expect(
                expression(),
                "Expected expression after comparison operator",
            ),
        )),
        |(left, operator, right)| {
            Some(ComparisonCondition {
                left: left?,
                operator: operator?,
                right: right.flatten()?,
            })
        },
    )
}

fn condition<'a>() -> impl FnMut(TokenInput<'a>) -> IResult<TokenInput<'a>, OrError<Condition>> {
    alt((
        map(odd_condition(), |c| c.map(Condition::Odd)),
        map(comparison_condition(), |c| {
            c.map(Condition::Comparison).into()
        }),
    ))
}

fn assignment_statement<'a>(
) -> impl FnMut(TokenInput<'a>) -> IResult<TokenInput<'a>, Option<AssignmentStatement>> {
    map(
        tuple((
            ident(),
            expect(token(TokenType::ColonEqual), "Expected ':=' after name"),
            expect(expression(), "Expected expression after ':='"),
        )),
        |(variable, _, expression)| {
            Some(AssignmentStatement {
                variable,
                expression: expression.flatten()?,
            })
        },
    )
}

fn procedure_call_statement<'a>(
) -> impl FnMut(TokenInput<'a>) -> IResult<TokenInput<'a>, Option<Token>> {
    preceded(
        token(TokenType::KwCall),
        expect(ident(), "Expected procedure name after 'call'"),
    )
}

fn read_statement<'a>() -> impl FnMut(TokenInput<'a>) -> IResult<TokenInput<'a>, Option<Token>> {
    preceded(
        token(TokenType::QuestionMark),
        expect(ident(), "Expected variable name after '?'"),
    )
}

fn write_statement<'a>() -> impl FnMut(TokenInput<'a>) -> IResult<TokenInput<'a>, Option<Expression>>
{
    map(
        tuple((
            token(TokenType::ExclamationMark),
            expect(expression(), "Expected expression after '!'"),
        )),
        |(_, expression)| Some(expression.flatten()?),
    )
}

fn prefixed_statement<'a>(
) -> impl FnMut(TokenInput<'a>) -> IResult<TokenInput<'a>, OrError<Statement>> {
    preceded(
        token(TokenType::Semicolon),
        map(expect(statement, "Expected statement after ';'"), |s| {
            OrError::from(s).flatten()
        }),
    )
}

fn block_statement<'a>(
) -> impl FnMut(TokenInput<'a>) -> IResult<TokenInput<'a>, Option<Vec<OrError<Statement>>>> {
    map(
        tuple((
            token(TokenType::KwBegin),
            expect(map(statement, Box::new), "Expected statement after 'begin'"),
            many0(prefixed_statement()),
            expect(
                token(TokenType::KwEnd),
                "Expected 'end' after block statement body",
            ),
        )),
        |(_, first, rest, _)| {
            Some({
                let mut statements = vec![first.map(|f| *f)?];
                statements.extend(rest);
                statements
            })
        },
    )
}

fn if_statement<'a>() -> impl FnMut(TokenInput<'a>) -> IResult<TokenInput<'a>, Option<IfStatement>>
{
    map(
        tuple((
            token(TokenType::KwIf),
            expect(condition(), "Expected condition after 'if'"),
            expect(token(TokenType::KwThen), "Expected 'then' after condition"),
            expect(map(statement, Box::new), "Expected statement after 'then'"),
        )),
        |(_, condition, _, body)| {
            let condition = OrError::from(condition).flatten();
            let body = OrError::from(body).map(|b| *b).flatten().map(Box::new);
            Some(IfStatement { condition, body })
        },
    )
}

fn while_statement<'a>(
) -> impl FnMut(TokenInput<'a>) -> IResult<TokenInput<'a>, Option<WhileStatement>> {
    map(
        tuple((
            token(TokenType::KwWhile),
            expect(condition(), "Expected condition after 'while'"),
            expect(
                token(TokenType::KwDo),
                "Expected 'do' after while loop condition",
            ),
            expect(
                map(statement, Box::new),
                "Expected while loop body after 'do'",
            ),
        )),
        |(_, condition, _, body)| {
            Some(WhileStatement {
                condition: OrError::from(condition).flatten(),
                body: OrError::from(body).map(|b| *b).flatten().map(Box::new),
            })
        },
    )
}

fn statement(input: TokenInput) -> IResult<TokenInput, OrError<Statement>> {
    map(
        opt(alt((
            map(assignment_statement(), |s| {
                OrError::from(s).map(Statement::Assignment)
            }),
            map(procedure_call_statement(), |s| {
                OrError::from(s).map(Statement::ProcedureCall)
            }),
            map(read_statement(), |s| OrError::from(s).map(Statement::Read)),
            map(write_statement(), |s| {
                OrError::from(s).map(Statement::Write)
            }),
            map(block_statement(), |s| {
                OrError::from(s).map(Statement::Block)
            }),
            map(if_statement(), |s| OrError::from(s).map(Statement::If)),
            map(while_statement(), |s| {
                OrError::from(s).map(Statement::While)
            }),
        ))),
        |s| s.unwrap_or(OrError::Ok(Statement::Empty)),
    )(input)
}

fn block<'a>() -> impl FnMut(TokenInput<'a>) -> IResult<TokenInput<'a>, Block> {
    map(
        tuple((
            opt(const_section()),
            opt(var_section()),
            many0(procedure),
            statement,
        )),
        |(const_section, var_section, procedures, body)| Block {
            const_section,
            var_section,
            procedures: procedures.into_iter().map(Into::into).collect(),
            body,
        },
    )
}

fn program<'a>(tokens: &'a [Token], state: &'a State) -> IResult<TokenInput<'a>, Option<Block>> {
    expect(
        all_consuming(terminated(
            block(),
            expect(token(TokenType::Period), "Expected '.' at end of program"),
        )),
        "Early end of input",
    )
    .parse(TokenInput::new(tokens, state))
}

pub(crate) fn parse(tokens: &[Token]) -> (SyntaxTree, Vec<Error>) {
    let errors = RefCell::new(Vec::new());
    let tree = stacker::grow(1024 * 1024, || {
        program(tokens, &State(&errors))
            .map(|(_, root)| SyntaxTree {
                root: root.map(Box::new).into(),
            })
            .expect("parser cannot fail")
    });
    (tree, errors.into_inner())
}
