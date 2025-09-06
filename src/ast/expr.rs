use crate::{
    ast::{
        compiler::Compiler,
        parser::{InvalidAssignmentTarget, ParseError},
        Parser, Spanned,
    },
    lexer::{Span, TokenKind, TokenValue},
    vm,
};

pub(super) fn parse(
    parser: &mut impl Parser,
    errors: &mut Vec<ParseError>,
) -> Option<Spanned<Expression>> {
    let span = parser.begin_span();
    let expr = Expression::parse(parser);
    let span = parser.end_span(span);

    match expr {
        Ok(expr) => Some(Spanned::new(span, expr)),
        Err(error) => {
            errors.push(error);
            // TODO: synchronize
            parser.advance();
            None
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
    Negate,
    LogicalNot,
}
impl UnaryOp {
    fn diag_symbol(&self) -> &'static str {
        match self {
            UnaryOp::Negate => "-",
            UnaryOp::LogicalNot => "!",
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOp {
    Mul,
    Div,
    Add,
    Sub,
    Lt,
    Gt,
    Lte,
    Gte,
    Eq,
    Neq,
}
impl BinaryOp {
    fn diag_symbol(&self) -> &'static str {
        match self {
            BinaryOp::Mul => "*",
            BinaryOp::Div => "/",
            BinaryOp::Add => "+",
            BinaryOp::Sub => "-",
            BinaryOp::Lt => "<",
            BinaryOp::Gt => ">",
            BinaryOp::Lte => "<=",
            BinaryOp::Gte => ">=",
            BinaryOp::Eq => "==",
            BinaryOp::Neq => "!=",
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    LiteralString(String),
    LiteralNumber(f64),
    LiteralBool(bool),
    LiteralNil,
    Group(Box<Spanned<Expression>>),
    Unary(UnaryOp, Box<Spanned<Expression>>),
    Binary(Box<Spanned<Expression>>, BinaryOp, Box<Spanned<Expression>>),
    Variable(String),
    Assignement(Spanned<String>, Box<Spanned<Expression>>),
}
impl Expression {
    pub fn diag_print(&self, out: &mut impl std::io::Write) -> std::io::Result<()> {
        match self {
            Expression::LiteralString(v) => write!(out, r"{v}"),
            Expression::LiteralNumber(v) => write!(out, "{v:?}"),
            Expression::LiteralBool(true) => write!(out, "true"),
            Expression::LiteralBool(false) => write!(out, "false"),
            Expression::LiteralNil => write!(out, "nil"),
            Expression::Group(e) => {
                write!(out, "(group ")?;
                e.value().diag_print(out)?;
                write!(out, ")")
            }
            Expression::Unary(op, expr) => {
                write!(out, "({} ", op.diag_symbol())?;
                expr.value().diag_print(out)?;
                write!(out, ")")
            }
            Expression::Binary(left, op, right) => {
                write!(out, "({} ", op.diag_symbol())?;
                left.value().diag_print(out)?;
                write!(out, " ")?;
                right.value().diag_print(out)?;
                write!(out, ")")
            }
            Expression::Variable(name) => {
                write!(out, "{name}")
            }
            Expression::Assignement(name, value) => {
                write!(out, "{} = ", name.value())?;
                value.value().diag_print(out)?;
                write!(out, ")")
            }
        }
    }

    pub(super) fn compile(self, span: Span, compiler: &mut Compiler) {
        match self {
            Expression::LiteralString(value) => {
                compiler.push(span, vm::Instruction::PushString(value))
            }
            Expression::LiteralNumber(value) => {
                compiler.push(span, vm::Instruction::PushNumber(value))
            }
            Expression::LiteralBool(value) => compiler.push(span, vm::Instruction::PushBool(value)),
            Expression::LiteralNil => compiler.push(span, vm::Instruction::PushNil),
            Expression::Group(spanned) => spanned.value.compile(span, compiler),
            Expression::Unary(unary_op, spanned) => {
                spanned.value.compile(spanned.span, compiler);
                match unary_op {
                    UnaryOp::Negate => compiler.push(span, vm::Instruction::Negate),
                    UnaryOp::LogicalNot => compiler.push(span, vm::Instruction::LogicalNot),
                }
            }
            Expression::Binary(left, op, right) => {
                left.value.compile(left.span, compiler);
                right.value.compile(right.span, compiler);
                match op {
                    BinaryOp::Mul => compiler.push(span, vm::Instruction::Mul),
                    BinaryOp::Div => compiler.push(span, vm::Instruction::Div),
                    BinaryOp::Add => compiler.push(span, vm::Instruction::Add),
                    BinaryOp::Sub => compiler.push(span, vm::Instruction::Sub),
                    BinaryOp::Lt => compiler.push(span, vm::Instruction::Less),
                    BinaryOp::Gt => {
                        compiler.push(span.clone(), vm::Instruction::LessOrEqual);
                        compiler.push(span, vm::Instruction::LogicalNot);
                    }
                    BinaryOp::Lte => compiler.push(span, vm::Instruction::LessOrEqual),
                    BinaryOp::Gte => {
                        compiler.push(span.clone(), vm::Instruction::Less);
                        compiler.push(span, vm::Instruction::LogicalNot);
                    }
                    BinaryOp::Eq => {
                        compiler.push(span, vm::Instruction::Equal);
                    }
                    BinaryOp::Neq => {
                        compiler.push(span.clone(), vm::Instruction::Equal);
                        compiler.push(span, vm::Instruction::LogicalNot);
                    }
                }
            }
            Expression::Variable(name) => {
                compiler.read_variable(Spanned::new(span, &name));
            }
            Expression::Assignement(name, value) => {
                value.value.compile(value.span, compiler);
                compiler.write_variable(Spanned::new(name.span(), name.value()));
            }
        }
    }

    pub(super) fn parse(parser: &mut impl Parser) -> Result<Expression, ParseError> {
        Self::parse_assignment(parser)
    }

    fn parse_assignment(parser: &mut impl Parser) -> Result<Expression, ParseError> {
        let left_span = parser.begin_span();
        let left = Self::parse_equality(parser)?;
        let left_span = parser.end_span(left_span);

        let invalid_target_error = parser.error_here(InvalidAssignmentTarget);

        if parser.take(TokenKind::Equal).is_some() {
            let right_span = parser.begin_span();
            let right = Self::parse_assignment(parser)?;
            let right_span = parser.end_span(right_span);

            if let Expression::Variable(name) = left {
                return Ok(Expression::Assignement(
                    Spanned::new(left_span, name),
                    Spanned::boxed(right_span, right),
                ));
            }

            return Err(invalid_target_error);
        }

        Ok(left)
    }

    fn parse_equality(parser: &mut impl Parser) -> Result<Expression, ParseError> {
        Self::parse_binary(parser, Self::parse_comparison, |v| match v {
            TokenValue::EqualEqual => Some(BinaryOp::Eq),
            TokenValue::BangEqual => Some(BinaryOp::Neq),
            _ => None,
        })
    }

    fn parse_comparison(parser: &mut impl Parser) -> Result<Expression, ParseError> {
        Self::parse_binary(parser, Self::parse_term, |v| match v {
            TokenValue::Less => Some(BinaryOp::Lt),
            TokenValue::Greater => Some(BinaryOp::Gt),
            TokenValue::LessEqual => Some(BinaryOp::Lte),
            TokenValue::GreaterEqual => Some(BinaryOp::Gte),
            _ => None,
        })
    }

    fn parse_term(parser: &mut impl Parser) -> Result<Expression, ParseError> {
        Self::parse_binary(parser, Self::parse_factor, |v| match v {
            TokenValue::Plus => Some(BinaryOp::Add),
            TokenValue::Minus => Some(BinaryOp::Sub),
            _ => None,
        })
    }

    fn parse_factor(parser: &mut impl Parser) -> Result<Expression, ParseError> {
        Self::parse_binary(parser, Self::parse_unary, |v| match v {
            TokenValue::Star => Some(BinaryOp::Mul),
            TokenValue::Slash => Some(BinaryOp::Div),
            _ => None,
        })
    }

    fn parse_binary<P: Parser>(
        parser: &mut P,
        parse_expr: impl Fn(&mut P) -> Result<Expression, ParseError>,
        ops: impl Fn(&TokenValue) -> Option<BinaryOp>,
    ) -> Result<Expression, ParseError> {
        let left_span_begin = parser.begin_span();
        let mut expr = parse_expr(parser)?;
        let mut left_span = parser.end_span(left_span_begin);

        while let Some(op) = parser.take_if(&ops) {
            let right_span = parser.begin_span();
            let right = parse_expr(parser)?;
            let right_span = parser.end_span(right_span);

            expr = Expression::Binary(
                Spanned::boxed(left_span, expr),
                op,
                Spanned::boxed(right_span, right),
            );
            left_span = parser.end_span(left_span_begin);
        }

        Ok(expr)
    }

    fn parse_unary(parser: &mut impl Parser) -> Result<Expression, ParseError> {
        if let Some(op) = parser.take_if(|v| match v {
            TokenValue::Bang => Some(UnaryOp::LogicalNot),
            TokenValue::Minus => Some(UnaryOp::Negate),
            _ => None,
        }) {
            let span = parser.begin_span();
            let expr = Self::parse_unary(parser)?;
            let span = parser.end_span(span);
            Ok(Expression::Unary(op, Spanned::boxed(span, expr)))
        } else {
            Self::parse_primary(parser)
        }
    }

    fn parse_primary(parser: &mut impl Parser) -> Result<Expression, ParseError> {
        let literal = parser.take_if(|v| match v {
            TokenValue::KwTrue => Some(Expression::LiteralBool(true)),
            TokenValue::KwFalse => Some(Expression::LiteralBool(false)),
            TokenValue::KwNil => Some(Expression::LiteralNil),
            TokenValue::Number(lit) => Some(Expression::LiteralNumber(*lit)),
            TokenValue::String(lit) => Some(Expression::LiteralString(lit.clone())),
            TokenValue::Identifier(name) => Some(Expression::Variable(name.clone())),
            _ => None,
        });

        if let Some(literal) = literal {
            Ok(literal)
        } else {
            parser.consume(TokenKind::LParen, " - no other expession options to parse")?;
            let span = parser.begin_span();
            let expr = Self::parse(parser)?;
            let span = parser.end_span(span);
            parser.consume(TokenKind::RParen, "after expression")?;
            Ok(Expression::Group(Spanned::boxed(span, expr)))
        }
    }
}
