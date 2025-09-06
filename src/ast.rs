use std::ops::Range;

use crate::{
    ast::{
        compiler::Compiler,
        parser::{ParseError, Parser, ParserState},
    },
    lexer::{LexerError, Span, Token},
    vm,
};

mod compiler;
mod expr;
mod parser;
mod stmt;

pub use expr::*;
pub use stmt::*;
// lalrpop_mod!(grammar);

pub fn parse_expression<'inp>(
    lexer: impl IntoIterator<Item = Result<Token<'inp>, LexerError>>,
    errs: &mut Vec<String>,
) -> Program {
    let mut state = ParserState::new(lexer.into_iter());
    let mut errors = Vec::new();
    let expr = expr::parse(&mut state, &mut errors);
    errs.extend(errors.into_iter().map(|e| e.to_string()));

    let mut program = Program { exprs: Vec::new() };
    if let Some(expr) = expr {
        let span = expr.span();
        program
            .exprs
            .push(Spanned::new(span, Statement::Print(expr)));
    }
    program
}

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
    pub exprs: Vec<Spanned<Statement>>,
}
impl Program {
    fn parse(parser: &mut impl Parser, errors: &mut Vec<ParseError>) -> Self {
        let mut exprs = Vec::new();

        while !parser.is_at_end() {
            if let Some(expr) = stmt::parse(parser, errors) {
                exprs.push(expr);
            }
        }

        Self { exprs }
    }

    pub fn compile(self) -> vm::Program {
        let mut compiler = Compiler::new();
        for expr in self.exprs {
            let span = expr.span();
            expr.value.compile(span.clone(), &mut compiler);
        }

        compiler.finalize()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Spanned<T> {
    value: T,
    span: Span,
}
impl<T> Spanned<T> {
    fn new(span: Span, value: T) -> Self {
        Spanned { value, span }
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
        self.span.range.clone()
    }

    pub fn span(&self) -> Span {
        self.span.clone()
    }
}
