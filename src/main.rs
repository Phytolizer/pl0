use std::io;
use std::path::PathBuf;

mod data;
mod lex;
mod parse;
mod tree;

use lex::lex;
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
    let tokens = lex(
        PathBuf::from("example.m"),
        "
            VAR x, squ;

            PROCEDURE square;
            BEGIN
               squ := x * x
            END;

            BEGIN
               x := 1;
               WHILE x <= 10 DO
               BEGIN
                  CALL square;
                  ! squ;
                  x := x + 1
               END
            END.
        ",
    )?;
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
