use std::iter::Enumerate;
use std::num::NonZeroUsize;
use std::ops::Range;
use std::ops::RangeFrom;
use std::ops::RangeFull;
use std::ops::RangeTo;

use nom::bytes::complete::take;
use nom::combinator::map;
use nom::combinator::verify;
use nom::sequence::pair;
use nom::sequence::terminated;
use nom::IResult;
use nom::InputIter;
use nom::InputLength;
use nom::InputTake;
use nom::Needed;
use nom::Parser;
use nom::Slice;

use crate::data::Token;
use crate::data::TokenType;
use crate::tree::Block;
use crate::tree::Program;
use crate::tree::SyntaxTree;

#[derive(Debug, thiserror::Error)]
pub(crate) enum ParseError {
    #[error("parsing: {0}")]
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

fn token<'a>(ty: TokenType) -> impl FnMut(TokenInput<'a>) -> IResult<TokenInput<'a>, Token> {
    map(
        verify(take(1usize), move |t: &TokenInput| {
            t.tokens[0].token_type == ty
        }),
        |t: TokenInput| t.tokens[0].clone(),
    )
}

fn block(input: TokenInput) -> IResult<TokenInput<'_>, Block> {
    todo!()
}

fn program(tokens: &[Token]) -> IResult<TokenInput, Program> {
    pair(block, token(TokenType::Period))
        .map(|(block, period_token)| Program {
            block,
            period_token,
        })
        .parse(TokenInput::new(tokens))
}

fn parse(tokens: &[Token]) -> crate::Result<SyntaxTree> {
    program(tokens)
        .map(|(_, root)| SyntaxTree {
            root: Box::new(root),
        })
        .map_err(|e| ParseError::Nom(e.to_string()).into())
}
