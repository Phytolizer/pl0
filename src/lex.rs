use std::io;
use std::iter::Peekable;
use std::path::PathBuf;
use std::str::Chars;

use crate::data::SourceLocation;
use crate::data::Token;
use crate::data::TokenType;

struct SourceCodeIterator<'src> {
    text: Peekable<Chars<'src>>,
    line: usize,
    column: usize,
}

impl<'src> SourceCodeIterator<'src> {
    fn new(text: &'src str) -> Self {
        Self {
            text: text.chars().peekable(),
            line: 1,
            column: 1,
        }
    }

    fn peek(&mut self) -> Option<&char> {
        self.text.peek()
    }
}

impl<'src> Iterator for SourceCodeIterator<'src> {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        let c = self.text.next()?;
        match c {
            '\n' => {
                self.line += 1;
                self.column = 1;
            }
            _ => {
                self.column += 1;
            }
        }
        Some(c)
    }
}

struct Lexer<'src> {
    file_path: PathBuf,
    source: SourceCodeIterator<'src>,
}

impl<'src> Lexer<'src> {
    fn new(file_path: PathBuf, text: &'src str) -> Self {
        Self {
            file_path,
            source: SourceCodeIterator::new(text),
        }
    }
}

pub(crate) fn lex(file_path: PathBuf, text: &str) -> Result<Vec<Token>, io::Error> {
    let lexer = Lexer::new(file_path, text);
    Ok(lexer.collect())
}

impl<'src> Iterator for Lexer<'src> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let line = self.source.line;
        let column = self.source.column;
        let kind = loop {
            match self.source.next()? {
                c if c.is_whitespace() => {}
                '.' => break TokenType::Period,
                '=' => break TokenType::Equal,
                ',' => break TokenType::Comma,
                ';' => break TokenType::Semicolon,
                ':' => match self.source.next() {
                    Some('=') => break TokenType::ColonEqual,
                    _ => break TokenType::Unknown(':'),
                },
                '?' => break TokenType::QuestionMark,
                '!' => break TokenType::ExclamationMark,
                '#' => break TokenType::Hash,
                '<' => match self.source.peek() {
                    Some('=') => {
                        self.source.next();
                        break TokenType::LessThanEqual;
                    }
                    _ => break TokenType::LessThan,
                },
                '>' => match self.source.peek() {
                    Some('=') => {
                        self.source.next();
                        break TokenType::GreaterThanEqual;
                    }
                    _ => break TokenType::GreaterThan,
                },
                '+' => break TokenType::Plus,
                '-' => break TokenType::Minus,
                '*' => break TokenType::Asterisk,
                '/' => break TokenType::Slash,
                '(' => break TokenType::LeftParen,
                ')' => break TokenType::RightParen,
                c if c.is_alphabetic() => {
                    let mut text = String::from(c);
                    loop {
                        match self.source.peek() {
                            Some(c) if c.is_alphanumeric() => {
                                text.push(*c);
                                self.source.next();
                            }
                            _ => break,
                        }
                    }
                    match text.to_uppercase().as_str() {
                        "BEGIN" => break TokenType::KwBegin,
                        "CALL" => break TokenType::KwCall,
                        "CONST" => break TokenType::KwConst,
                        "DO" => break TokenType::KwDo,
                        "END" => break TokenType::KwEnd,
                        "IF" => break TokenType::KwIf,
                        "ODD" => break TokenType::KwOdd,
                        "PROCEDURE" => break TokenType::KwProcedure,
                        "THEN" => break TokenType::KwThen,
                        "VAR" => break TokenType::KwVar,
                        "WHILE" => break TokenType::KwWhile,
                        _ => break TokenType::Ident(text),
                    }
                }
                c if c.is_digit(10) => {
                    let mut text = String::from(c);
                    loop {
                        match self.source.peek() {
                            Some(c) if c.is_digit(10) => {
                                text.push(*c);
                                self.source.next();
                            }
                            _ => break,
                        }
                    }
                    break TokenType::Number(text);
                }
                c => break TokenType::Unknown(c),
            }
        };
        Some(Token::new(
            kind,
            SourceLocation::new(self.file_path.clone(), line, column),
        ))
    }
}
