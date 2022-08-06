use std::fmt::Debug;
use std::fmt::Display;
use std::path::PathBuf;

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum TokenType {
    // Symbols
    Period,
    Equal,
    Comma,
    Semicolon,
    ColonEqual,
    QuestionMark,
    ExclamationMark,
    Hash,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
    Plus,
    Minus,
    Asterisk,
    Slash,
    LeftParen,
    RightParen,
    // Keywords
    KwBegin,
    KwCall,
    KwConst,
    KwDo,
    KwEnd,
    KwIf,
    KwOdd,
    KwProcedure,
    KwThen,
    KwVar,
    KwWhile,
    // Special tokens
    Ident(String),
    Number(String),
    Unknown(char),
}

#[derive(Debug, Clone)]
pub(crate) struct SourceLocation {
    pub(crate) file_path: PathBuf,
    pub(crate) line: usize,
    pub(crate) column: usize,
}

impl Display for SourceLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}:{}:{}",
            self.file_path.display(),
            self.line,
            self.column
        )
    }
}

impl SourceLocation {
    pub(crate) fn new(file_path: PathBuf, line: usize, column: usize) -> Self {
        Self {
            file_path,
            line,
            column,
        }
    }
}

#[derive(Clone)]
pub(crate) struct Token {
    pub(crate) token_type: TokenType,
    pub(crate) source_location: SourceLocation,
}

impl Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {:?}", self.source_location, self.token_type)
    }
}

impl Token {
    pub(crate) fn new(token_type: TokenType, source_location: SourceLocation) -> Self {
        Self {
            token_type,
            source_location,
        }
    }
}
