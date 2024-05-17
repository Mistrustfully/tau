use clap::{Parser, ValueEnum};
use frontend::{
    ast::Ast,
    layout::Layout,
    lexer::Lexer,
    typecheck::{core::TypeCheckerCore, TypeCheckState, TypecheckError},
};
use lalrpop_util::lalrpop_mod;
use std::{fs::File, io::Read, path::PathBuf};

lalrpop_mod!(#[allow(clippy::ptr_arg)] pub grammar);
pub mod frontend;
pub mod location;

#[derive(Parser)]
struct TauCLI {
    #[arg(value_name = "STAGE")]
    command: Commands,
    #[arg(value_name = "PATH")]
    file: PathBuf,
}

#[derive(Clone, Copy, ValueEnum)]
enum Commands {
    Run,
    Typecheck,
    Parse,
    Lex,
}

struct Stage<'input> {
    source: &'input str,
}

impl<'input> Stage<'input> {
    fn new(source: &'input str) -> Self {
        Self { source }
    }

    fn lex(&mut self) -> Layout<'input> {
        Layout::new(Lexer::new(self.source))
    }

    fn parse(&mut self) -> Ast<'input> {
        Ast::parse(self.source)
    }

    fn typecheck(&mut self) -> Result<TypeCheckerCore<'input>, TypecheckError> {
        let mut typechecker = TypeCheckState::default();
        typechecker.check_ast(self.parse())
    }

    fn run(&mut self) {
        todo!("Run not implemented")
    }
}

fn read_file(path: PathBuf) -> String {
    let mut text = String::new();

    File::open(path)
        .expect("Failed to open file!")
        .read_to_string(&mut text)
        .expect("Failed to read file to string");

    text
}
fn main() {
    let args = TauCLI::parse();
    let source = read_file(args.file);
    let mut stage = Stage::new(&source);

    match args.command {
        Commands::Run => stage.run(),

        Commands::Typecheck => {
            let res = stage.typecheck();
            println!("{:#?}", res)
        }

        Commands::Parse => {
            let ast = stage.parse();
            println!("{:#?}", ast);
        }

        Commands::Lex => {
            let layout = stage.lex();

            for t in layout.into_iter().flatten() {
                println!("{}\n{:?}\n", t.span, t.value);
            }
        }
    }
}
