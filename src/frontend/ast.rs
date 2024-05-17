use super::{layout::Layout, lexer::Lexer};
use crate::grammar;

// TODO: Output AST with span information for better error reporting

#[derive(Debug, PartialEq)]
pub struct Ast<'input>(pub Vec<Statement<'input>>);
impl<'input> Ast<'input> {
    pub fn parse(source: &str) -> Ast {
        let mut errors = Vec::new();
        let lexer = Lexer::new(source);
        let layout = Layout::new(lexer);

        let ast = grammar::AstParser::new()
            .parse(
                &mut errors,
                layout.map(|t| t.map(|v| (v.span.start, v.value, v.span.end))),
            )
            .expect("Failed to parse?");

        for err in errors {
            println!("{:#?}", err);
        }

        ast
    }
}

#[derive(Debug, PartialEq)]
pub enum Statement<'input> {
    Import(&'input str),
    Declaration {
        name: Identifier<'input>,
        patterns: Vec<Identifier<'input>>,
        expr: Expr<'input>,
        sig: Option<TypeExpr>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr<'input> {
    Literal(Literal<'input>),
    Identifier(Identifier<'input>),
    Application {
        lhs: Box<Expr<'input>>,
        rhs: Box<Expr<'input>>,
    },
    Prefix(Operator<'input>, Box<Expr<'input>>),
    Infix {
        lhs: Box<Expr<'input>>,
        op: Operator<'input>,
        rhs: Box<Expr<'input>>,
    },
    Lambda {
        params: Vec<Identifier<'input>>,
        func: Box<Expr<'input>>,
    },
    Error,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Operator<'input>(pub &'input str);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Identifier<'input>(pub &'input str);

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Literal<'input> {
    Float(f32),
    Int(i32),
    String(&'input str),
    Bool(bool),
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeExpr {
    String,
    Float,
    Int,
    Bool,
    Empty,
    Fn {
        lhs: Box<TypeExpr>,
        rhs: Box<TypeExpr>,
    },
    Error,
}

#[cfg(test)]
mod test {
    use crate::frontend::ast::{Expr, Identifier, Literal, Operator, Statement, TypeExpr};

    use super::Ast;

    #[test]
    fn parse_ast() {
        let source = r#"
            my_str = "foo"
            add_string a b =
                a + b
                : String -> String -> String

            foo_bar = add_str my_str "bar"
        "#;

        let ast = Ast::parse(source);
        let expected = Ast(vec![
            Statement::Declaration {
                name: Identifier("my_str"),
                patterns: vec![],
                expr: Expr::Literal(Literal::String("foo")),
                sig: None,
            },
            Statement::Declaration {
                name: Identifier("add_string"),
                patterns: vec![Identifier("a"), Identifier("b")],
                expr: Expr::Infix {
                    lhs: Box::new(Expr::Identifier(Identifier("a"))),
                    op: Operator("+"),
                    rhs: Box::new(Expr::Identifier(Identifier("b"))),
                },
                sig: Some(TypeExpr::Fn {
                    lhs: Box::new(TypeExpr::String),
                    rhs: Box::new(TypeExpr::Fn {
                        lhs: Box::new(TypeExpr::String),
                        rhs: Box::new(TypeExpr::String),
                    }),
                }),
            },
            Statement::Declaration {
                name: Identifier("foo_bar"),
                patterns: vec![],
                expr: Expr::Application {
                    lhs: Box::new(Expr::Application {
                        lhs: Box::new(Expr::Identifier(Identifier("add_str"))),
                        rhs: Box::new(Expr::Identifier(Identifier("my_str"))),
                    }),
                    rhs: Box::new(Expr::Literal(Literal::String("bar"))),
                },
                sig: None,
            },
        ]);

        assert!(
            ast == expected,
            "Did not get expected ast when parsing! Expected {:#?} got {:#?}",
            expected,
            ast
        )
    }
}
