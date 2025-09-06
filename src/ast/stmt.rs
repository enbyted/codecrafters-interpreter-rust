use crate::{
    ast::{compiler::Compiler, parser::ExpectedToken, Expression, ParseError, Parser, Spanned},
    lexer::{Span, TokenKind, TokenValue},
    vm,
};

pub(super) fn parse(
    parser: &mut impl Parser,
    errors: &mut Vec<ParseError>,
) -> Option<Spanned<Statement>> {
    let span = parser.begin_span();
    let stmt = Statement::parse_declarion(parser);
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
    VarDeclaration {
        name: Spanned<String>,
        value: Option<Spanned<Expression>>,
    },
}
impl Statement {
    pub(super) fn compile(self, span: Span, compiler: &mut Compiler) {
        match self {
            Statement::Expr(expr) => {
                // TODO: Check if the expression has side effects
                //       If not - we can skip it
                expr.value.compile(expr.span, compiler);
                compiler.push(span, vm::Instruction::Pop);
            }
            Statement::Print(expr) => {
                expr.value.compile(expr.span, compiler);
                compiler.push(span.clone(), vm::Instruction::Print);
                compiler.push(span, vm::Instruction::Pop);
            }
            Statement::VarDeclaration { name, value } => {
                compiler.declare_variable(Spanned::new(name.span(), &name.value));
                if let Some(value) = value {
                    value.value.compile(value.span, compiler);
                    compiler.write_variable(Spanned::new(name.span(), &name.value));
                }
            }
        }
    }

    fn parse_declarion(parser: &mut impl Parser) -> Result<Statement, ParseError> {
        if parser.take(TokenKind::KwVar).is_some() {
            Self::parse_var_declaration(parser)
        } else {
            Self::parse_statement(parser)
        }
    }

    fn parse_var_declaration(parser: &mut impl Parser) -> Result<Statement, ParseError> {
        let span = parser.begin_span();
        let name = if let Some(name) = parser.take_if(|v| match v {
            TokenValue::Identifier(v) => Some(v.to_string()),
            _ => None,
        }) {
            name
        } else {
            return Err(parser.error_here(ExpectedToken {
                token: TokenKind::Identifier,
                message: "Expect variable name.",
            }));
        };
        let span = parser.end_span(span);
        let value = if parser.take(TokenKind::Equal).is_some() {
            let span = parser.begin_span();
            let value = Expression::parse(parser)?;
            let span = parser.end_span(span);
            Some(Spanned::new(span, value))
        } else {
            None
        };
        parser.consume(TokenKind::Semicolon, "Expect ';' after value.")?;

        Ok(Statement::VarDeclaration {
            name: Spanned::new(span, name),
            value,
        })
    }

    fn parse_statement(parser: &mut impl Parser) -> Result<Statement, ParseError> {
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
