use std::io;
use std::path::PathBuf;

mod data;
mod lex;
mod tree;

use lex::lex;

#[derive(Debug, thiserror::Error)]
enum Error {
    #[error(transparent)]
    Io(#[from] io::Error),
}

type Result<T> = std::result::Result<T, Error>;

fn run() -> Result<()> {
    let tokens = lex(PathBuf::from("example.m"), "BEGIN VAR x := 1; END.")?;
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
