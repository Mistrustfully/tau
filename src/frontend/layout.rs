use super::lexer::{Lexer, LexicalError, SpannedToken, Token};
use crate::location::Location;

/// A wrapper around [crate::frontend::lexer::Lexer]
/// Applies layout rules to the lexed tokens to resolve grammar ambigiuities.
///
/// The applied transformations to the lexed content:
///
/// - If you see one of the layout keywords, (let, where, of, do), insert an open curly brace (right before the stuff that follows it)
/// - If you see something indented to the SAME level, insert a semicolon
/// - If you see something indented LESS, insert a closing curly brace
/// - If you see something unexpected in a list, like where, insert a closing brace before instead of a semicolon.
///
/// (See https://en.wikibooks.org/wiki/Haskell/Indentation)
///
/// Ex:
/// ```
///     foo = bar
///     baz a = "hello"
/// ```
///
/// Could also be parsed as:
/// ```
///     foo = bar baz
///     a = "hello"
/// ```
///
/// Instead the `OpenBlock`, `CloseBlock`, and `Semicolon` tokens get inserted during the lexing step, making the grammar unambiguous:
/// ```
///     foo = bar <Semicolon>
///     baz a = "hello" <Semicolon>
/// ```
/// TODO: Implement OpenBlock and CloseBlock
pub struct Layout<'input> {
    queued_tokens: Vec<SpannedToken<'input>>,
    offside: Vec<Location>,
    lexer: Lexer<'input>,
}

impl<'input> Layout<'input> {
    pub fn new(lexer: Lexer<'input>) -> Self {
        Self {
            lexer,
            offside: vec![],
            queued_tokens: vec![],
        }
    }

    fn layout_token(&mut self, token: SpannedToken<'input>) {
        self.queued_tokens.push(token);
        if let Some(offside) = self.offside.last() {
            if token.span.start.column == offside.column || token.value == Token::Eof {
                self.queued_tokens
                    .push(self.lexer.spanned_token(Token::Semicolon));
            }
        } else {
            self.offside.push(token.span.start)
        }
    }
}

impl<'input> Iterator for Layout<'input> {
    type Item = Result<SpannedToken<'input>, LexicalError>;
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if let Some(token) = self.queued_tokens.pop() {
                return if token.value == Token::Eof {
                    None
                } else {
                    Some(Ok(token))
                };
            }

            if let Some(Ok(token)) = self.lexer.next() {
                self.layout_token(token)
            }
        }
    }
}

#[cfg(test)]
mod test {
    use crate::frontend::{
        ast::Literal,
        lexer::{Lexer, Token},
    };

    use super::Layout;

    #[test]
    fn insert_semicolons() {
        let source = r#"
            main =
                1 + 1
                : Int

            main' a =
                main + a
                : Int -> Int
        "#;

        let lexer = Lexer::new(source);
        let layout = Layout::new(lexer).map(|v| v.unwrap().value);

        let expected_tokens = vec![
            Token::Identifier("main"),
            Token::Equal,
            Token::Literal(Literal::Int(1)),
            Token::Operator("+"),
            Token::Literal(Literal::Int(1)),
            Token::Colon,
            Token::Identifier("Int"),
            Token::Semicolon,
            Token::Identifier("main'"),
            Token::Identifier("a"),
            Token::Equal,
            Token::Identifier("main"),
            Token::Operator("+"),
            Token::Identifier("a"),
            Token::Colon,
            Token::Identifier("Int"),
            Token::RightArrow,
            Token::Identifier("Int"),
        ];

        for (actual, expected) in layout.zip(expected_tokens) {
            assert!(
                actual == expected,
                "Tokens did not match! Expected {:#?}, got {:#?}",
                expected,
                actual
            )
        }
    }
}
