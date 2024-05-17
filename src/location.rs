use std::fmt::{Debug, Display};

use codespan::{ByteIndex, ByteOffset, ColumnIndex, ColumnOffset, LineIndex, LineOffset};

#[derive(Copy, Clone, Default, Eq, PartialEq, Debug, Hash, Ord, PartialOrd)]
pub struct Location {
    pub line: LineIndex,
    pub column: ColumnIndex,
    pub absolute: ByteIndex,
}

impl Location {
    pub fn new() -> Self {
        Location {
            line: LineIndex(0),
            column: ColumnIndex(0),
            absolute: ByteIndex(0),
        }
    }

    pub fn shift(&mut self, ch: char) {
        if ch == '\n' {
            self.column = ColumnIndex(0);
            self.line += LineOffset(1);
        } else {
            self.column += ColumnOffset(1);
        }
        self.absolute += ByteOffset(1);
    }
}

impl Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Span<I> {
    pub start: I,
    pub end: I,
}

impl<I> Span<I> {
    fn new(start: I, end: I) -> Self {
        Span { start, end }
    }
}

impl<I: Display> Display for Span<I> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} - {}", self.start, self.end)
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Spanned<T, Pos: Clone + Copy + PartialEq> {
    pub span: Span<Pos>,
    pub value: T,
}

impl<T: Debug, Pos: Clone + Copy + Display + PartialEq> Display for Spanned<T, Pos> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}\n{:#?}", self.span.start, self.value)
    }
}

pub fn span<Pos>(start: Pos, end: Pos) -> Span<Pos>
where
    Pos: Ord,
{
    Span::new(start, end)
}

pub fn spanned<T, Pos: Clone + Copy + PartialEq>(span: Span<Pos>, value: T) -> Spanned<T, Pos> {
    Spanned { span, value }
}

pub fn spanned2<T, Pos: Clone + Copy>(start: Pos, end: Pos, value: T) -> Spanned<T, Pos>
where
    Pos: Ord,
{
    spanned(span(start, end), value)
}
