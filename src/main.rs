use std::io;

mod data;
mod lex;
mod parse;
mod tree;

use lex::lex_file;
use parse::parse;
use parse::ParseError;

#[derive(Debug, thiserror::Error)]
enum Error {
    #[error(transparent)]
    Io(#[from] io::Error),
    #[error(transparent)]
    Parse(#[from] ParseError),
}

type Result<T> = std::result::Result<T, Error>;

fn run() -> Result<()> {
    let tokens = lex_file("example.m")?;
    let tree = parse(&tokens)?;
    dbg!(tree);
    Ok(())
}

fn main() {
    match run() {
        Ok(()) => {}
        Err(e) => {
            eprintln!("{}", e);
            std::process::exit(1);
        }
    }
}
