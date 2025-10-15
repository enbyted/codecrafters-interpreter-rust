use std::sync::Arc;

use crate::{
    ast::{
        compiler::{Compiler, ScopeType},
        parser::ExpectedToken,
        Expression, ParseError, Parser, Spanned,
    },
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
    FunDeclaration {
        name: Spanned<String>,
        args: Vec<Spanned<String>>,
        body: Spanned<Box<Statement>>,
    },
    Block(Vec<Spanned<Statement>>),
    If {
        condition: Spanned<Expression>,
        then_branch: Spanned<Box<Statement>>,
        else_branch: Option<Spanned<Box<Statement>>>,
    },
    While {
        condition: Spanned<Expression>,
        body: Spanned<Box<Statement>>,
    },
    For {
        initializer: Option<Spanned<Box<Statement>>>,
        condition: Option<Spanned<Expression>>,
        increment: Option<Spanned<Expression>>,
        body: Spanned<Box<Statement>>,
    },
    Return(Option<Spanned<Expression>>),
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
                } else {
                    compiler.push(span, vm::Instruction::PushNil);
                }
                compiler.write_variable(Spanned::new(name.span(), &name.value));
            }
            Statement::FunDeclaration { name, args, body } => {
                compiler.declare_variable(Spanned::new(name.span(), &name.value));
                let mut inner = Compiler::new();
                inner.enter_scope(ScopeType::Function);
                let arity = args.len();
                for arg in args {
                    inner.declare_variable(Spanned::new(arg.span(), &arg.value));
                }
                body.value.compile(body.span.clone(), &mut inner);
                inner.push(body.span.clone(), vm::Instruction::PushNil);
                inner.push(body.span, vm::Instruction::Return);
                inner.exit_scope();
                let chunk = inner.into_chunk();
                compiler.push(
                    name.span(),
                    vm::Instruction::PushFunction {
                        chunk: Arc::new(chunk),
                        arity,
                        name: name.value.clone(),
                    },
                );
                compiler.write_variable(Spanned::new(name.span(), &name.value));
            }
            Statement::Block(statements) => {
                compiler.enter_scope(ScopeType::Block);
                for statement in statements {
                    statement.value.compile(statement.span, compiler);
                }
                compiler.exit_scope();
            }
            Statement::If {
                condition,
                then_branch,
                else_branch,
            } => {
                condition.value.compile(condition.span.clone(), compiler);
                let then_patch = compiler.jump(condition.span.clone(), vm::JumpCondition::Falsy);
                then_branch.value.compile(then_branch.span, compiler);
                if let Some(else_branch) = else_branch {
                    let else_patch = compiler.jump(condition.span, vm::JumpCondition::Always);
                    compiler.patch_jump(then_patch, compiler.ip());
                    else_branch.value.compile(else_branch.span, compiler);
                    compiler.patch_jump(else_patch, compiler.ip());
                } else {
                    compiler.patch_jump(then_patch, compiler.ip());
                }
            }
            Statement::While { condition, body } => {
                let loop_start = compiler.ip();
                condition.value.compile(condition.span.clone(), compiler);
                let condition_patch =
                    compiler.jump(condition.span.clone(), vm::JumpCondition::Falsy);
                body.value.compile(body.span.clone(), compiler);
                let end_patch = compiler.jump(body.span, vm::JumpCondition::Always);
                compiler.patch_jump(end_patch, loop_start);
                compiler.patch_jump(condition_patch, compiler.ip());
            }
            Statement::For {
                initializer,
                condition,
                increment,
                body,
            } => {
                compiler.enter_scope(ScopeType::ForLoop);
                if let Some(initializer) = initializer {
                    initializer.value.compile(initializer.span, compiler);
                }
                let loop_start = compiler.ip();
                let condition_patch = if let Some(condition) = condition {
                    condition.value.compile(condition.span.clone(), compiler);
                    Some(compiler.jump(condition.span, vm::JumpCondition::Falsy))
                } else {
                    None
                };
                body.value.compile(body.span.clone(), compiler);
                if let Some(increment) = increment {
                    increment.value.compile(increment.span.clone(), compiler);
                }
                let end_patch = compiler.jump(body.span, vm::JumpCondition::Always);
                compiler.patch_jump(end_patch, loop_start);
                if let Some(condition_patch) = condition_patch {
                    compiler.patch_jump(condition_patch, compiler.ip());
                }

                compiler.exit_scope();
            }
            Statement::Return(expr) => {
                if let Some(expr) = expr {
                    expr.value.compile(expr.span, compiler);
                } else {
                    compiler.push(span.clone(), vm::Instruction::PushNil);
                }
                compiler.push(span, vm::Instruction::Return);
            }
        }
    }

    fn parse_declarion(parser: &mut impl Parser) -> Result<Statement, ParseError> {
        if parser.take(TokenKind::KwFun).is_some() {
            Self::parse_fun_declaration(parser)
        } else if parser.take(TokenKind::KwVar).is_some() {
            Self::parse_var_declaration(parser)
        } else {
            Self::parse_statement(parser)
        }
    }

    fn parse_fun_declaration(parser: &mut impl Parser) -> Result<Statement, ParseError> {
        let span = parser.begin_span();
        let name = if let Some(name) = parser.take_if(|v| match v {
            TokenValue::Identifier(v) => Some(v.to_string()),
            _ => None,
        }) {
            name
        } else {
            return Err(parser.error_here(ExpectedToken {
                token: TokenKind::Identifier,
                message: "Expect function name.",
            }));
        };
        let span = parser.end_span(span);

        parser.consume(TokenKind::LParen, "Expect '(' after function name.")?;
        let args = if parser.take(TokenKind::RParen).is_some() {
            Vec::new()
        } else {
            let mut args = Vec::new();
            loop {
                let arg_span = parser.begin_span();
                let arg = parser
                    .take_if(|v| match v {
                        TokenValue::Identifier(ident) => Some(ident.clone()),
                        _ => None,
                    })
                    .ok_or_else(|| {
                        parser.error_here(ExpectedToken {
                            token: TokenKind::Identifier,
                            message: "Expect function argument name.",
                        })
                    })?;
                let arg_span = parser.end_span(arg_span);
                args.push(Spanned::new(arg_span, arg));

                if parser.take(TokenKind::Comma).is_none() {
                    break;
                }
            }

            parser.consume(TokenKind::RParen, "Expect ')' after function arguments.")?;
            args
        };
        parser.consume(TokenKind::LBrace, "Expect '{' after function arguments.")?;
        let body_span = parser.begin_span();
        let body = Self::parse_block(parser)?;
        let body_span = parser.end_span(body_span);
        Ok(Statement::FunDeclaration {
            name: Spanned::new(span, name),
            args,
            body: Spanned::new(body_span, Box::new(body)),
        })
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
        parser.consume(
            TokenKind::Semicolon,
            "Expect ';' after variable declaration.",
        )?;

        Ok(Statement::VarDeclaration {
            name: Spanned::new(span, name),
            value,
        })
    }

    fn parse_statement(parser: &mut impl Parser) -> Result<Statement, ParseError> {
        if parser.take(TokenKind::KwPrint).is_some() {
            Self::parse_print(parser)
        } else if parser.take(TokenKind::KwReturn).is_some() {
            Self::parse_return(parser)
        } else if parser.take(TokenKind::KwWhile).is_some() {
            Self::parse_while(parser)
        } else if parser.take(TokenKind::KwFor).is_some() {
            Self::parse_for(parser)
        } else if parser.take(TokenKind::LBrace).is_some() {
            Self::parse_block(parser)
        } else if parser.take(TokenKind::KwIf).is_some() {
            Self::parse_if(parser)
        } else {
            Self::parse_expr(parser)
        }
    }

    fn parse_return(parser: &mut impl Parser) -> Result<Statement, ParseError> {
        let expr = if parser.take(TokenKind::Semicolon).is_some() {
            None
        } else {
            let expr = parser.spanned(Expression::parse)?;
            parser.consume(TokenKind::Semicolon, "Expect ';' after return value.")?;
            Some(expr)
        };
        Ok(Statement::Return(expr))
    }

    fn parse_if(parser: &mut impl Parser) -> Result<Statement, ParseError> {
        parser.consume(TokenKind::LParen, "Expect '(' after 'if'.")?;
        let condition_span = parser.begin_span();
        let condition = Expression::parse(parser)?;
        let condition_span = parser.end_span(condition_span);
        parser.consume(TokenKind::RParen, "Expect ')' after condition.")?;
        let then_branch_span = parser.begin_span();
        let then_branch = Self::parse_statement(parser)?;
        let then_branch_span = parser.end_span(then_branch_span);
        let else_branch = if parser.take(TokenKind::KwElse).is_some() {
            let else_branch_span = parser.begin_span();
            let else_branch = Self::parse_statement(parser)?;
            let else_branch_span = parser.end_span(else_branch_span);
            Some(Spanned::new(else_branch_span, Box::new(else_branch)))
        } else {
            None
        };

        Ok(Statement::If {
            condition: Spanned::new(condition_span, condition),
            then_branch: Spanned::new(then_branch_span, Box::new(then_branch)),
            else_branch,
        })
    }

    fn parse_block(parser: &mut impl Parser) -> Result<Statement, ParseError> {
        let mut statements = Vec::new();
        while parser.current().value() != TokenKind::RBrace && !parser.is_at_end() {
            let span = parser.begin_span();
            let statement = Self::parse_declarion(parser)?;
            let span = parser.end_span(span);
            statements.push(Spanned::new(span, statement));
        }
        parser.consume(TokenKind::RBrace, "Expect '}' after block.")?;
        Ok(Statement::Block(statements))
    }

    fn parse_for(parser: &mut impl Parser) -> Result<Statement, ParseError> {
        parser.consume(TokenKind::LParen, "Expect '(' after 'for'.")?;
        let initializer = if parser.take(TokenKind::Semicolon).is_some() {
            None
        } else if parser.take(TokenKind::KwVar).is_some() {
            let span = parser.begin_span();
            let declaration = Self::parse_var_declaration(parser)?;
            let span = parser.end_span(span);
            Some(Spanned::new(span, Box::new(declaration)))
        } else {
            let span = parser.begin_span();
            let expr = Self::parse_expr(parser)?;
            let span = parser.end_span(span);
            Some(Spanned::new(span, Box::new(expr)))
        };

        let condition = if parser.take(TokenKind::Semicolon).is_some() {
            None
        } else {
            let span = parser.begin_span();
            let condition = Expression::parse(parser)?;
            let span = parser.end_span(span);
            parser.consume(TokenKind::Semicolon, "Expect ';' after condition.")?;
            Some(Spanned::new(span, condition))
        };

        let increment = if parser.current().value() != &TokenValue::RParen {
            let span = parser.begin_span();
            let increment = Expression::parse(parser)?;
            let span = parser.end_span(span);
            Some(Spanned::new(span, increment))
        } else {
            None
        };
        parser.consume(TokenKind::RParen, "Expect ')' after for clauses.")?;
        let body_span = parser.begin_span();
        let body = Self::parse_statement(parser)?;
        let body_span = parser.end_span(body_span);
        Ok(Statement::For {
            initializer,
            condition,
            increment,
            body: Spanned::new(body_span, Box::new(body)),
        })
    }

    fn parse_while(parser: &mut impl Parser) -> Result<Statement, ParseError> {
        parser.consume(TokenKind::LParen, "Expect '(' after 'while'.")?;
        let condition_span = parser.begin_span();
        let condition = Expression::parse(parser)?;
        let condition_span = parser.end_span(condition_span);
        parser.consume(TokenKind::RParen, "Expect ')' after condition.")?;
        let body_span = parser.begin_span();
        let body = Self::parse_statement(parser)?;
        let body_span = parser.end_span(body_span);
        Ok(Statement::While {
            condition: Spanned::new(condition_span, condition),
            body: Spanned::new(body_span, Box::new(body)),
        })
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
