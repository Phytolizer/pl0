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

impl SourceLocation {
    pub(crate) fn new(file_path: PathBuf, line: usize, column: usize) -> Self {
        Self {
            file_path,
            line,
            column,
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Token {
    pub(crate) token_type: TokenType,
    pub(crate) source_location: SourceLocation,
}

impl Token {
    pub(crate) fn new(token_type: TokenType, source_location: SourceLocation) -> Self {
        Self {
            token_type,
            source_location,
        }
    }
}
