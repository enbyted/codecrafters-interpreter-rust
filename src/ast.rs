use std::ops::Range;

use crate::{
    ast::parser::{ParseError, Parser, ParserState},
    lexer::{LexerError, Span, Token},
};

mod expr;
mod parser;

pub use expr::*;
// lalrpop_mod!(grammar);

pub fn parse<'inp>(
    lexer: impl IntoIterator<Item = Result<Token<'inp>, LexerError>>,
    errs: &mut Vec<String>,
) -> Program {
    let mut state = ParserState::new(lexer.into_iter());
    let mut errors = Vec::new();
    let ret = Program::parse(&mut state, &mut errors);
    errs.extend(errors.into_iter().map(|e| e.to_string()));
    ret
}

pub struct Program {
    pub exprs: Vec<Spanned<Expression>>,
}
impl Program {
    fn parse(parser: &mut impl Parser, errors: &mut Vec<ParseError>) -> Self {
        let mut exprs = Vec::new();

        while !parser.is_at_end() {
            if let Some(expr) = expr::parse(parser, errors) {
                exprs.push(expr);
            }
        }

        Self { exprs }
    }
}

pub struct Spanned<T> {
    value: T,
    span: Range<usize>,
}
impl<T> Spanned<T> {
    fn new(span: Span, value: T) -> Self {
        Spanned {
            value,
            span: span.range,
        }
    }

    fn boxed(span: Span, value: T) -> Box<Self> {
        Box::new(Self::new(span, value))
    }

    pub fn into_value(self) -> T {
        self.value
    }

    pub fn value(&self) -> &T {
        &self.value
    }

    pub fn range(&self) -> Range<usize> {
        self.span.clone()
    }
}
