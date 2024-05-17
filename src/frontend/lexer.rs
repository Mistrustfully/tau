use std::{iter::Peekable, str::CharIndices};

use crate::location::{spanned2, Location, Spanned};
use thiserror::Error;

use super::ast::Literal;

#[derive(Debug, Error)]
pub enum LexicalError {
    #[error("Unexpected token, expected {expected:?} found {found:?} ({location:?}")]
    UnexpectedToken {
        expected: char,
        found: char,
        location: Location,
    },

    #[error("Unexpected EoF")]
    UnexpectedEoF,

    #[error("Invalid number")]
    InvalidNumber,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Token<'input> {
    Identifier(&'input str),
    Operator(&'input str),

    Literal(Literal<'input>),

    // Symbols:
    /// `=`
    Equal,
    /// `(`
    LParen,
    /// `)`
    RParen,
    /// \`
    Grave,
    /// `->`
    RightArrow,
    /// `\`
    Backslash,
    /// `:`
    Colon,

    // Keywords:
    /// `import`
    Import,

    /// Used for parsing based on layout, indicates the ending of a statement.
    /// See [crate::frontend::layout::Layout]
    Semicolon,
    /// Used for parsing based on layout, indicates the opening of a block.
    /// See [crate::frontend::layout::Layout]
    OpenBlock,
    /// Used for parsing based on layout, indicates the closing of a block.
    /// See [crate::frontend::layout::Layout]
    CloseBlock,

    /// Indicates the end of a file, removed during the layouting step.
    Eof,
}

pub type SpannedToken<'input> = Spanned<Token<'input>, Location>;

pub struct CharLocations<'input> {
    location: Location,
    chars: Peekable<CharIndices<'input>>,
}

impl<'input> CharLocations<'input> {
    pub fn new(input: &'input str) -> Self {
        CharLocations {
            location: Location::new(),
            chars: input.char_indices().peekable(),
        }
    }

    pub fn peek(&mut self) -> Option<(Location, char)> {
        self.chars.peek().map(|(_, ch)| {
            let location = self.location;
            // location.shift(*ch);
            (location, *ch)
        })
    }
}

impl<'input> Iterator for CharLocations<'input> {
    type Item = (Location, char);
    fn next(&mut self) -> Option<Self::Item> {
        self.chars.next().map(|(_, ch)| {
            let location = self.location;

            self.location.shift(ch);
            (location, ch)
        })
    }
}

fn is_identifier(ch: char, first: bool) -> bool {
    if first {
        ch.is_alphabetic() || ch == '_'
    } else {
        ch.is_alphabetic() || ch.is_ascii_digit() || ch == '\'' || ch == '_'
    }
}

fn is_symbol(ch: char) -> bool {
    matches!(
        ch,
        '|' | '<' | '>' | '@' | '+' | '-' | '*' | '/' | '~' | '!'
    )
}

pub struct Lexer<'input> {
    chars: CharLocations<'input>,
    input: &'input str,
}

impl<'input> Lexer<'input> {
    pub fn new(input: &'input str) -> Self {
        Lexer {
            chars: CharLocations::new(input),
            input,
        }
    }

    pub fn bump(&mut self) -> Option<(Location, char)> {
        self.chars.next()
    }

    pub fn peek(&mut self) -> Option<(Location, char)> {
        self.chars.peek()
    }

    pub fn next_index(&mut self) -> Location {
        self.peek().map_or(self.chars.location, |(l, _)| l)
    }

    pub fn slice(&mut self, start: Location, end: Location) -> &'input str {
        &self.input[start.absolute.into()..end.absolute.into()]
    }

    pub fn spanned_token(&mut self, token: Token<'input>) -> SpannedToken<'input> {
        spanned2(self.chars.location, self.next_index(), token)
    }

    pub fn create_token(
        &mut self,
        token: Token<'input>,
    ) -> Option<Result<SpannedToken<'input>, LexicalError>> {
        Some(Ok(self.spanned_token(token)))
    }

    fn take_until<F>(&mut self, start: Location, mut predicate: F) -> (Location, &'input str)
    where
        F: FnMut(char) -> bool,
    {
        while let Some((end, ch)) = self.peek() {
            if predicate(ch) {
                return (end, self.slice(start, end));
            } else {
                self.bump();
            }
        }

        let next = self.next_index();
        (next, self.slice(start, next))
    }

    fn string_literal(&mut self, start: Location) -> Result<SpannedToken<'input>, LexicalError> {
        let content_start = self.next_index();
        let (_, _) = self.take_until(start, |c| c == '"');
        let content_end = self.next_index();
        self.bump();
        let (end, _) = self.bump().ok_or(LexicalError::UnexpectedEoF)?;

        Ok(spanned2(
            start,
            end,
            Token::Literal(Literal::String(self.slice(content_start, content_end))),
        ))
    }

    fn float_literal(&mut self, start: Location) -> Result<SpannedToken<'input>, LexicalError> {
        let (end, _) = self.take_until(start, |c| !c.is_ascii_digit());
        let number = self
            .slice(start, end)
            .parse::<f32>()
            .map_err(|_| LexicalError::InvalidNumber)?;

        Ok(spanned2(start, end, Token::Literal(Literal::Float(number))))
    }

    fn number_literal(&mut self, start: Location) -> Result<SpannedToken<'input>, LexicalError> {
        let (end, _) = self.take_until(start, |c| !c.is_ascii_digit());
        if let Some((_loc, peek)) = self.peek() {
            if peek == '.' {
                self.bump();
                return self.float_literal(start);
            }
        }

        let number = self
            .slice(start, end)
            .parse::<i32>()
            .map_err(|_| LexicalError::InvalidNumber)?;

        Ok(spanned2(start, end, Token::Literal(Literal::Int(number))))
    }

    fn identifier(&mut self, start: Location) -> Result<SpannedToken<'input>, LexicalError> {
        let (end, str) = self.take_until(start, |c| !is_identifier(c, false));
        let token = match str {
            "import" => Token::Import,
            "true" => Token::Literal(Literal::Bool(true)),
            "false" => Token::Literal(Literal::Bool(false)),
            _ => Token::Identifier(str),
        };

        Ok(spanned2(start, end, token))
    }

    fn operator(&mut self, start: Location) -> Result<SpannedToken<'input>, LexicalError> {
        let (end, str) = self.take_until(start, |c| !is_symbol(c));
        let token = match str {
            "->" => Token::RightArrow,
            _ => Token::Operator(str),
        };

        Ok(spanned2(start, end, token))
    }

    fn comment(&mut self, start: Location) {
        self.take_until(start, |c| c == '\n');
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Result<SpannedToken<'input>, LexicalError>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let Some(next) = self.bump() else {
                return self.create_token(Token::Eof);
            };

            let start = next.0;
            let char = next.1;

            match char {
                '"' => return Some(self.string_literal(start)),
                '=' => return self.create_token(Token::Equal),
                '(' => return self.create_token(Token::LParen),
                ')' => return self.create_token(Token::RParen),
                '`' => return self.create_token(Token::Grave),
                ';' => return self.create_token(Token::Semicolon),
                '\\' => return self.create_token(Token::Backslash),
                ':' => return self.create_token(Token::Colon),

                '#' => self.comment(start),

                '.' => return Some(self.float_literal(start)),
                c if c.is_ascii_digit() => return Some(self.number_literal(start)),
                c if is_identifier(c, true) => return Some(self.identifier(start)),
                c if is_symbol(c) => return Some(self.operator(start)),
                c if c.is_whitespace() => continue,
                c => {
                    return Some(Err(LexicalError::UnexpectedToken {
                        expected: ' ',
                        found: c,
                        location: start,
                    }))
                }
            }
        }
    }
}

#[cfg(test)]
mod test {
    use crate::{
        frontend::ast::Literal,
        location::{spanned2, Location, Spanned},
    };

    use super::{Lexer, LexicalError, Token};
    use codespan::{ByteIndex, ColumnIndex, LineIndex};
    use std::iter::{Map, TakeWhile};

    fn lex<'input>(
        source: &'input str,
    ) -> TakeWhile<
        Map<
            Lexer<'input>,
            impl FnMut(Result<Spanned<Token<'input>, Location>, LexicalError>) -> Token<'input>,
        >,
        impl FnMut(&Token<'input>) -> bool,
    > {
        let lexer = Lexer::new(source);
        lexer
            .map(|v| v.unwrap().value)
            .take_while(|v| v != &Token::Eof)
    }

    fn loc(c: u32, l: u32, a: u32) -> Location {
        Location {
            column: ColumnIndex(c),
            line: LineIndex(l),
            absolute: ByteIndex(a),
        }
    }

    #[test]
    fn span() {
        let source = r#""foo" "bar" "baz"
|>
# comments are skipped
-> # again
100
"#;
        let result = Lexer::new(source).take_while(|v| {
            if let Ok(v) = v {
                v.value != Token::Eof
            } else {
                true
            }
        });

        let expected_tokens = vec![
            spanned2(
                loc(0, 0, 0),
                loc(5, 0, 5),
                Token::Literal(Literal::String("foo")),
            ),
            spanned2(
                loc(6, 0, 6),
                loc(11, 0, 11),
                Token::Literal(Literal::String("bar")),
            ),
            spanned2(
                loc(12, 0, 12),
                loc(17, 0, 17),
                Token::Literal(Literal::String("baz")),
            ),
            spanned2(loc(0, 1, 18), loc(2, 1, 20), Token::Operator("|>")),
            spanned2(loc(0, 3, 44), loc(2, 3, 46), Token::RightArrow),
            spanned2(
                loc(0, 4, 55),
                loc(3, 4, 58),
                Token::Literal(Literal::Int(100)),
            ),
        ];

        let expected_content = vec!["\"foo\"", "\"bar\"", "\"baz\"", "|>", "->", "100"];

        for ((actual, expected), expected_slice) in
            result.zip(expected_tokens).zip(expected_content)
        {
            let Ok(actual) = actual else {
                panic!("Failed to lex token, err: {}", actual.unwrap_err())
            };

            assert!(
                actual == expected,
                "Did not lex string properly, expected {:#?}, got {:#?}",
                expected,
                actual
            );

            let actual_slice =
                &source[actual.span.start.absolute.into()..actual.span.end.absolute.into()];

            assert!(
                actual_slice == expected_slice,
                "Expected content slice doesn't match, expected {:?}, got {:?}",
                expected_slice,
                actual_slice
            )
        }
    }

    #[test]
    fn literals() {
        let source = r#"
        "I am a string literal"
        -1.0
        0.1
        .5
        100
        false
        true
        "#;

        let result = lex(source);
        let expected_tokens = vec![
            Token::Literal(Literal::String("I am a string literal")),
            Token::Operator("-"),
            Token::Literal(Literal::Float(1.0)),
            Token::Literal(Literal::Float(0.1)),
            Token::Literal(Literal::Float(0.5)),
            Token::Literal(Literal::Int(100)),
            Token::Literal(Literal::Bool(false)),
            Token::Literal(Literal::Bool(true)),
        ];

        for (actual, expected) in result.zip(expected_tokens) {
            assert!(
                actual == expected,
                "Did not lex string properly, expected {:#?}, got {:#?}",
                expected,
                actual
            )
        }
    }

    #[test]
    fn lex_content() {
        let source = r#"
            (1 * 2.0 + "hello" / .05)
            # Comment
            -> |> identifier
        "#;

        let result = lex(source);

        let expected_tokens = vec![
            Token::LParen,
            Token::Literal(Literal::Int(1)),
            Token::Operator("*"),
            Token::Literal(Literal::Float(2.0)),
            Token::Operator("+"),
            Token::Literal(Literal::String("hello")),
            Token::Operator("/"),
            Token::Literal(Literal::Float(0.05)),
            Token::RParen,
            Token::RightArrow,
            Token::Operator("|>"),
            Token::Identifier("identifier"),
        ];

        for (actual, expected) in result.zip(expected_tokens) {
            assert!(
                actual == expected,
                "Did not lex string properly, expected {:#?}, got {:#?}",
                expected,
                actual
            )
        }
    }
}
