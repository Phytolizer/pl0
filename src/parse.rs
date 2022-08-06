use std::cell::RefCell;
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
use crate::tree::BlockStatement;
use crate::tree::ComparisonCondition;
use crate::tree::Condition;
use crate::tree::ConstEntries;
use crate::tree::ConstEntry;
use crate::tree::ConstSection;
use crate::tree::Expression;
use crate::tree::Factor;
use crate::tree::IfStatement;
use crate::tree::OddCondition;
use crate::tree::ParenthesizedFactor;
use crate::tree::PrefixedFactor;
use crate::tree::PrefixedTerm;
use crate::tree::Procedure;
use crate::tree::ProcedureCallStatement;
use crate::tree::Program;
use crate::tree::ReadStatement;
use crate::tree::Statement;
use crate::tree::StatementInner;
use crate::tree::SyntaxTree;
use crate::tree::Term;
use crate::tree::VarEntries;
use crate::tree::VarEntry;
use crate::tree::VarSection;
use crate::tree::WhileStatement;
use crate::tree::WriteStatement;

#[derive(Debug)]
pub(crate) struct Error {
    pub(crate) location: SourceLocation,
    pub(crate) message: String,
}

impl Error {
    fn new(location: SourceLocation, message: String) -> Self {
        Self { location, message }
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
        Err(nom::Err::Error(input) | nom::Err::Failure(input)) => {
            let err = Error::new(
                input.input.tokens[0].source_location.clone(),
                error_msg.to_string(),
            );
            input.input.state.report_error(err);
            Ok((input.input, None))
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

fn const_entries<'a>() -> impl FnMut(TokenInput<'a>) -> IResult<TokenInput<'a>, ConstEntries> {
    map(
        pair(const_entry(), many0(prefixed_const_entry())),
        |(first, rest)| ConstEntries {
            entries: {
                let mut entries = vec![first];
                entries.extend(rest.into_iter().filter_map(|e| e));
                entries
            },
        },
    )
}

fn const_section<'a>() -> impl FnMut(TokenInput<'a>) -> IResult<TokenInput<'a>, ConstSection> {
    map(
        tuple((
            token(TokenType::KwConst),
            expect(const_entries(), "Expected constant name after 'const'"),
            expect(token(TokenType::Semicolon), "Expected ';' after constants"),
        )),
        |(const_kw, entries, semicolon_token)| ConstSection {
            const_kw,
            entries,
            semicolon_token,
        },
    )
}

fn var_entry<'a>() -> impl FnMut(TokenInput<'a>) -> IResult<TokenInput<'a>, VarEntry> {
    map(ident(), |name| VarEntry { name })
}

fn prefixed_var_entry<'a>() -> impl FnMut(TokenInput<'a>) -> IResult<TokenInput<'a>, VarEntry> {
    map(pair(token(TokenType::Comma), var_entry()), |(_, entry)| {
        entry
    })
}

fn var_entries<'a>() -> impl FnMut(TokenInput<'a>) -> IResult<TokenInput<'a>, VarEntries> {
    map(
        pair(var_entry(), many0(prefixed_var_entry())),
        |(first, rest)| VarEntries {
            entries: {
                let mut entries = vec![first];
                entries.extend(rest.into_iter());
                entries
            },
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

fn procedure(input: TokenInput) -> IResult<TokenInput, Procedure> {
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
        |(_, name, _, body, _)| Procedure { name, body },
    )(input)
}

fn prefixed_factor<'a>() -> impl FnMut(TokenInput<'a>) -> IResult<TokenInput<'a>, PrefixedFactor> {
    map(
        pair(
            alt((token(TokenType::Asterisk), token(TokenType::Slash))),
            expect(factor(), "Expected factor after '*' or '/'"),
        ),
        |(operator, factor)| PrefixedFactor { operator, factor },
    )
}

fn parenthesized_factor(input: TokenInput) -> IResult<TokenInput, Option<ParenthesizedFactor>> {
    map(
        tuple((
            token(TokenType::LeftParen),
            expect(map(expression(), Box::new), "Expected expression after '('"),
            expect(
                token(TokenType::RightParen),
                "Expected ')' after expression",
            ),
        )),
        |(_, expression, _)| expression.map(|e| ParenthesizedFactor { expression: e }),
    )(input)
}

fn factor<'a>() -> impl FnMut(TokenInput<'a>) -> IResult<TokenInput<'a>, Factor> {
    alt((
        map(ident(), Factor::Variable),
        map(number(), Factor::Number),
        map(parenthesized_factor, Factor::Parenthesized),
    ))
}

fn prefixed_term<'a>() -> impl FnMut(TokenInput<'a>) -> IResult<TokenInput<'a>, PrefixedTerm> {
    map(
        pair(
            alt((token(TokenType::Plus), token(TokenType::Minus))),
            term(),
        ),
        |(operator, term)| PrefixedTerm { operator, term },
    )
}

fn term<'a>() -> impl FnMut(TokenInput<'a>) -> IResult<TokenInput<'a>, Term> {
    map(pair(factor(), many0(prefixed_factor())), |(first, rest)| {
        Term { first, rest }
    })
}

fn expression<'a>() -> impl FnMut(TokenInput<'a>) -> IResult<TokenInput<'a>, Expression> {
    map(
        tuple((
            opt(alt((token(TokenType::Plus), token(TokenType::Minus)))),
            term(),
            many0(prefixed_term()),
        )),
        |(sign, first, rest)| Expression { sign, first, rest },
    )
}

fn odd_condition<'a>() -> impl FnMut(TokenInput<'a>) -> IResult<TokenInput<'a>, Option<OddCondition>>
{
    map(
        tuple((
            token(TokenType::KwOdd),
            expect(expression(), "Expected expression after 'odd'"),
        )),
        |(_, expression)| expression.map(|e| OddCondition { expression: e }),
    )
}

fn comparison_condition<'a>(
) -> impl FnMut(TokenInput<'a>) -> IResult<TokenInput<'a>, ComparisonCondition> {
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
        |(left, operator, right)| ComparisonCondition {
            left,
            operator,
            right,
        },
    )
}

fn condition<'a>() -> impl FnMut(TokenInput<'a>) -> IResult<TokenInput<'a>, Condition> {
    alt((
        map(odd_condition(), Condition::Odd),
        map(comparison_condition(), Condition::Comparison),
    ))
}

fn assignment_statement<'a>(
) -> impl FnMut(TokenInput<'a>) -> IResult<TokenInput<'a>, AssignmentStatement> {
    map(
        tuple((
            ident(),
            expect(token(TokenType::ColonEqual), "Expected ':=' after name"),
            expect(expression(), "Expected expression after ':='"),
        )),
        |(variable, _, expression)| AssignmentStatement {
            variable,
            expression,
        },
    )
}

fn procedure_call_statement<'a>(
) -> impl FnMut(TokenInput<'a>) -> IResult<TokenInput<'a>, ProcedureCallStatement> {
    map(tuple((token(TokenType::KwCall), ident())), |(_, name)| {
        ProcedureCallStatement { name }
    })
}

fn read_statement<'a>() -> impl FnMut(TokenInput<'a>) -> IResult<TokenInput<'a>, ReadStatement> {
    map(
        tuple((
            token(TokenType::QuestionMark),
            expect(ident(), "Expected variable name after '?'"),
        )),
        |(_, name)| ReadStatement { name },
    )
}

fn write_statement<'a>() -> impl FnMut(TokenInput<'a>) -> IResult<TokenInput<'a>, WriteStatement> {
    map(
        tuple((
            token(TokenType::ExclamationMark),
            expect(expression(), "Expected expression after '!'"),
        )),
        |(_, expression)| WriteStatement { expression },
    )
}

fn prefixed_statement<'a>(
) -> impl FnMut(TokenInput<'a>) -> IResult<TokenInput<'a>, Option<Statement>> {
    preceded(
        token(TokenType::Semicolon),
        expect(statement, "Expected statement after ';'"),
    )
}

fn block_statement<'a>() -> impl FnMut(TokenInput<'a>) -> IResult<TokenInput<'a>, BlockStatement> {
    map(
        tuple((
            token(TokenType::KwBegin),
            map(statement, Box::new),
            many0(prefixed_statement()),
            token(TokenType::KwEnd),
        )),
        |(_, first, rest, _)| BlockStatement {
            statements: {
                let mut statements = vec![*first];
                statements.extend(rest.into_iter().filter_map(|s| s));
                statements
            },
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
            let condition = condition?;
            let body = body?;
            Some(IfStatement { condition, body })
        },
    )
}

fn while_statement<'a>() -> impl FnMut(TokenInput<'a>) -> IResult<TokenInput<'a>, WhileStatement> {
    map(
        tuple((
            token(TokenType::KwWhile),
            condition(),
            token(TokenType::KwDo),
            map(statement, Box::new),
        )),
        |(_, condition, _, body)| WhileStatement { condition, body },
    )
}

fn statement(input: TokenInput) -> IResult<TokenInput, Statement> {
    map(
        opt(alt((
            map(assignment_statement(), StatementInner::Assignment),
            map(procedure_call_statement(), StatementInner::ProcedureCall),
            map(read_statement(), StatementInner::Read),
            map(write_statement(), StatementInner::Write),
            map(block_statement(), StatementInner::Block),
            map(if_statement(), StatementInner::If),
            map(while_statement(), StatementInner::While),
        ))),
        |inner| Statement { inner },
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
            procedures,
            body,
        },
    )
}

fn program<'a>(tokens: &'a [Token], state: &'a State) -> IResult<TokenInput<'a>, Program> {
    all_consuming(pair(
        block(),
        expect(token(TokenType::Period), "Expected '.' at end of program"),
    ))
    .map(|(block, _)| Program { block })
    .parse(TokenInput::new(tokens, state))
}

pub(crate) fn parse(tokens: &[Token]) -> (SyntaxTree, Vec<Error>) {
    let errors = RefCell::new(Vec::new());
    let tree = stacker::grow(1024 * 1024, || {
        program(tokens, &State(&errors))
            .map(|(_, root)| SyntaxTree {
                root: Box::new(root),
            })
            .expect("parser cannot fail")
    });
    (tree, errors.into_inner())
}
