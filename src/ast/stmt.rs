use crate::{
    ast::{Expression, ParseError, Parser, Spanned},
    lexer::{Span, TokenKind},
    vm,
};

pub(super) fn parse(
    parser: &mut impl Parser,
    errors: &mut Vec<ParseError>,
) -> Option<Spanned<Statement>> {
    let span = parser.begin_span();
    let stmt = Statement::parse(parser);
    let span = parser.end_span(span);

    match stmt {
        Ok(stmt) => Some(Spanned::new(span, stmt)),
        Err(error) => {
            errors.push(error);
            // TODO: synchronize
            parser.advance();
            None
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Expr(Spanned<Expression>),
    Print(Spanned<Expression>),
}
impl Statement {
    pub(super) fn compile(self, span: Span, instructions: &mut Vec<Spanned<vm::Instruction>>) {
        match self {
            Statement::Expr(expr) => {
                expr.value.compile(expr.span, instructions);
                instructions.push(Spanned::new(span, vm::Instruction::Pop));
            }
            Statement::Print(expr) => {
                expr.value.compile(expr.span, instructions);
                instructions.push(Spanned::new(span.clone(), vm::Instruction::Print));
                instructions.push(Spanned::new(span, vm::Instruction::Pop));
            }
        }
    }
    fn parse(parser: &mut impl Parser) -> Result<Statement, ParseError> {
        if parser.take(TokenKind::KwPrint).is_some() {
            Self::parse_print(parser)
        } else {
            Self::parse_expr(parser)
        }
    }

    fn parse_print(parser: &mut impl Parser) -> Result<Statement, ParseError> {
        let span = parser.begin_span();
        let expr = Expression::parse(parser)?;
        let span = parser.end_span(span);
        parser.consume(TokenKind::Semicolon, "Expect ';' after value.")?;
        Ok(Statement::Print(Spanned::new(span, expr)))
    }

    fn parse_expr(parser: &mut impl Parser) -> Result<Statement, ParseError> {
        let span = parser.begin_span();
        let expr = Expression::parse(parser)?;
        let span = parser.end_span(span);
        parser.consume(TokenKind::Semicolon, "Expect ';' after value.")?;
        Ok(Statement::Expr(Spanned::new(span, expr)))
    }
}
