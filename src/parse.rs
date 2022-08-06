use std::error::Error;
use std::fmt::Display;
use std::iter::Enumerate;
use std::num::NonZeroUsize;
use std::ops::Range;
use std::ops::RangeFrom;
use std::ops::RangeFull;
use std::ops::RangeTo;

use nom::branch::alt;
use nom::bytes::complete::take;
use nom::combinator::map;
use nom::combinator::opt;
use nom::combinator::verify;
use nom::multi::many0;
use nom::sequence::pair;
use nom::sequence::tuple;
use nom::IResult;
use nom::InputIter;
use nom::InputLength;
use nom::InputTake;
use nom::Needed;
use nom::Parser;
use nom::Slice;

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
use crate::tree::PrefixedConstEntry;
use crate::tree::PrefixedFactor;
use crate::tree::PrefixedStatement;
use crate::tree::PrefixedTerm;
use crate::tree::PrefixedVarEntry;
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

#[derive(Debug, thiserror::Error)]
pub(crate) enum ParseError {
    #[error("{0}")]
    Nom(String),
}

#[derive(Debug, Clone, Copy)]
struct TokenInput<'a> {
    tokens: &'a [Token],
    start: usize,
    end: usize,
}

impl<'a> TokenInput<'a> {
    fn new(tokens: &'a [Token]) -> Self {
        Self {
            tokens,
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
            start: 0,
            end: count,
        }
    }

    fn take_split(&self, count: usize) -> (Self, Self) {
        let (prefix, suffix) = self.tokens.split_at(count);
        (Self::new(suffix), Self::new(prefix))
    }
}

impl<'a> Slice<Range<usize>> for TokenInput<'a> {
    fn slice(&self, range: Range<usize>) -> Self {
        Self {
            tokens: self.tokens.slice(range.clone()),
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
        tuple((ident(), token(TokenType::Equal), number())),
        |(name, equal_token, value)| ConstEntry {
            name,
            equal_token,
            value,
        },
    )
}

fn prefixed_const_entry<'a>(
) -> impl FnMut(TokenInput<'a>) -> IResult<TokenInput<'a>, PrefixedConstEntry> {
    map(
        pair(token(TokenType::Comma), const_entry()),
        |(comma_token, entry)| PrefixedConstEntry { comma_token, entry },
    )
}

fn const_entries<'a>() -> impl FnMut(TokenInput<'a>) -> IResult<TokenInput<'a>, ConstEntries> {
    map(
        pair(const_entry(), many0(prefixed_const_entry())),
        |(first, rest)| ConstEntries { first, rest },
    )
}

fn const_section<'a>() -> impl FnMut(TokenInput<'a>) -> IResult<TokenInput<'a>, ConstSection> {
    map(
        tuple((
            token(TokenType::KwConst),
            const_entries(),
            token(TokenType::Semicolon),
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

fn prefixed_var_entry<'a>(
) -> impl FnMut(TokenInput<'a>) -> IResult<TokenInput<'a>, PrefixedVarEntry> {
    map(
        pair(token(TokenType::Comma), var_entry()),
        |(comma_token, entry)| PrefixedVarEntry { comma_token, entry },
    )
}

fn var_entries<'a>() -> impl FnMut(TokenInput<'a>) -> IResult<TokenInput<'a>, VarEntries> {
    map(
        pair(var_entry(), many0(prefixed_var_entry())),
        |(first, rest)| VarEntries { first, rest },
    )
}

fn var_section<'a>() -> impl FnMut(TokenInput<'a>) -> IResult<TokenInput<'a>, VarSection> {
    map(
        tuple((
            token(TokenType::KwVar),
            var_entries(),
            token(TokenType::Semicolon),
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
            ident(),
            token(TokenType::Semicolon),
            map(block(), Box::new),
            token(TokenType::Semicolon),
        )),
        |(procedure_kw, name, first_semicolon_token, block, second_semicolon_token)| Procedure {
            procedure_kw,
            name,
            first_semicolon_token,
            block,
            second_semicolon_token,
        },
    )(input)
}

fn prefixed_factor<'a>() -> impl FnMut(TokenInput<'a>) -> IResult<TokenInput<'a>, PrefixedFactor> {
    map(
        pair(
            alt((token(TokenType::Asterisk), token(TokenType::Slash))),
            factor(),
        ),
        |(operator, factor)| PrefixedFactor { operator, factor },
    )
}

fn parenthesized_factor(input: TokenInput) -> IResult<TokenInput, ParenthesizedFactor> {
    map(
        tuple((
            token(TokenType::LeftParen),
            map(expression(), Box::new),
            token(TokenType::RightParen),
        )),
        |(left_parenthesis_token, expression, right_parenthesis_token)| ParenthesizedFactor {
            left_parenthesis_token,
            expression,
            right_parenthesis_token,
        },
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

fn odd_condition<'a>() -> impl FnMut(TokenInput<'a>) -> IResult<TokenInput<'a>, OddCondition> {
    map(
        tuple((token(TokenType::KwOdd), expression())),
        |(odd_kw, expression)| OddCondition { odd_kw, expression },
    )
}

fn comparison_condition<'a>(
) -> impl FnMut(TokenInput<'a>) -> IResult<TokenInput<'a>, ComparisonCondition> {
    map(
        tuple((
            expression(),
            alt((
                token(TokenType::Equal),
                token(TokenType::Hash),
                token(TokenType::LessThan),
                token(TokenType::LessThanEqual),
                token(TokenType::GreaterThan),
                token(TokenType::GreaterThanEqual),
            )),
            expression(),
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
        tuple((ident(), token(TokenType::ColonEqual), expression())),
        |(variable, colon_equal_token, expression)| AssignmentStatement {
            variable,
            colon_equal_token,
            expression,
        },
    )
}

fn procedure_call_statement<'a>(
) -> impl FnMut(TokenInput<'a>) -> IResult<TokenInput<'a>, ProcedureCallStatement> {
    map(
        tuple((token(TokenType::KwCall), ident())),
        |(call_kw, name)| ProcedureCallStatement { call_kw, name },
    )
}

fn read_statement<'a>() -> impl FnMut(TokenInput<'a>) -> IResult<TokenInput<'a>, ReadStatement> {
    map(
        tuple((token(TokenType::QuestionMark), ident())),
        |(question_mark_token, name)| ReadStatement {
            question_mark_token,
            name,
        },
    )
}

fn write_statement<'a>() -> impl FnMut(TokenInput<'a>) -> IResult<TokenInput<'a>, WriteStatement> {
    map(
        tuple((token(TokenType::ExclamationMark), expression())),
        |(exclamation_mark_token, expression)| WriteStatement {
            exclamation_mark_token,
            expression,
        },
    )
}

fn prefixed_statement<'a>(
) -> impl FnMut(TokenInput<'a>) -> IResult<TokenInput<'a>, PrefixedStatement> {
    map(
        pair(token(TokenType::Semicolon), statement),
        |(semicolon_token, statement)| PrefixedStatement {
            semicolon_token,
            statement,
        },
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
        |(begin_kw, first, rest, end_kw)| BlockStatement {
            begin_kw,
            first,
            rest,
            end_kw,
        },
    )
}

fn if_statement<'a>() -> impl FnMut(TokenInput<'a>) -> IResult<TokenInput<'a>, IfStatement> {
    map(
        tuple((
            token(TokenType::KwIf),
            condition(),
            token(TokenType::KwThen),
            map(statement, Box::new),
        )),
        |(if_kw, condition, then_kw, body)| IfStatement {
            if_kw,
            condition,
            then_kw,
            body,
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
        |(while_kw, condition, do_kw, body)| WhileStatement {
            while_kw,
            condition,
            do_kw,
            body,
        },
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

fn program(tokens: &[Token]) -> IResult<TokenInput, Program> {
    pair(block(), token(TokenType::Period))
        .map(|(block, period_token)| Program {
            block,
            period_token,
        })
        .parse(TokenInput::new(tokens))
}

pub(crate) fn parse(tokens: &[Token]) -> crate::Result<SyntaxTree> {
    program(tokens)
        .map(|(_, root)| SyntaxTree {
            root: Box::new(root),
        })
        .map_err(|e| ParseError::Nom(e.to_string()).into())
}
