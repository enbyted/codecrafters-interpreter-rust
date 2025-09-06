use std::collections::HashMap;

use thiserror::Error;

use crate::{ast::Spanned, lexer::Span};

#[derive(Debug)]
pub(crate) enum Instruction {
    // +1
    PushNil,
    // +1
    PushBool(bool),
    // +1
    PushString(String),
    // +1
    PushNumber(f64),
    // -1 +1
    Negate,
    // -1 +1
    LogicalNot,
    // -2 +1
    Add,
    // -2 +1
    Sub,
    // -2 +1
    Mul,
    // -2 +1
    Div,
    // -2 +1
    Less,
    // -2 +1
    LessOrEqual,
    // -2 +1
    Equal,
    // 0
    Print,
    // -1
    Pop,
    // +1
    ReadGlobal(String),
    // -1
    WriteGlobal(String),
}

pub struct Program {
    pub(crate) instructions: Vec<Spanned<Instruction>>,
}

#[derive(Debug, PartialEq, Clone)]
enum Value {
    Nil,
    Bool(bool),
    String(String),
    Number(f64),
}

pub trait ExecutionEnv {
    fn print(&mut self, value: &str);
}

#[derive(Debug, Error)]
#[error("{message}\n[line {line}]")]
pub struct RuntimeError {
    line: usize,
    message: String,
}
impl RuntimeError {
    fn new(span: Span, message: impl Into<String>) -> Self {
        Self {
            line: span.start_line(),
            message: message.into(),
        }
    }
}
trait RuntimeErrorExt<T> {
    fn with_message(self, message: impl Into<String>) -> Result<T, RuntimeError>;
}
impl<T> RuntimeErrorExt<T> for Result<T, RuntimeError> {
    fn with_message(self, message: impl Into<String>) -> Result<T, RuntimeError> {
        self.map_err(|e| RuntimeError {
            line: e.line,
            message: message.into(),
        })
    }
}

pub struct Vm<'env> {
    env: &'env mut dyn ExecutionEnv,
    stack: Vec<Value>,
    program: Vec<Spanned<Instruction>>,
    pc: usize,
    globals: HashMap<String, Value>,
}
impl<'env> Vm<'env> {
    pub fn new(env: &'env mut dyn ExecutionEnv, program: Program) -> Self {
        Self {
            env,
            stack: Vec::new(),
            program: program.instructions,
            pc: 0,
            globals: HashMap::new(),
        }
    }

    pub fn run(&mut self) -> Result<(), RuntimeError> {
        while let Some(instruction) = self.program.get(self.pc) {
            match instruction.value() {
                Instruction::PushNil => {
                    self.stack.push(Value::Nil);
                }
                Instruction::PushBool(value) => {
                    self.stack.push(Value::Bool(*value));
                }
                Instruction::PushString(value) => {
                    self.stack.push(Value::String(value.clone()));
                }
                Instruction::PushNumber(value) => {
                    self.stack.push(Value::Number(*value));
                }
                Instruction::Print => match self.stack.last() {
                    Some(Value::String(value)) => self.env.print(value),
                    Some(Value::Number(value)) => self.env.print(&format!("{value}")),
                    Some(Value::Nil) => self.env.print("nil"),
                    Some(Value::Bool(true)) => self.env.print("true"),
                    Some(Value::Bool(false)) => self.env.print("false"),
                    None => todo!("Handle runtime errors"),
                },
                Instruction::Pop => {
                    self.stack.pop();
                }
                Instruction::Negate => {
                    let value = self
                        .pop_number(instruction.span())
                        .with_message("Operand must be a number.")?;
                    self.stack.push(Value::Number(-value));
                }
                Instruction::LogicalNot => {
                    let value = self.pop_boolean(instruction.span())?;
                    self.stack.push(Value::Bool(!value));
                }
                Instruction::Add => {
                    let span = instruction.span();
                    let right = self.pop_value(span.clone())?;
                    let left = self.pop_value(span.clone())?;
                    match (left, right) {
                        (Value::Number(left), Value::Number(right)) => {
                            self.stack.push(Value::Number(left + right));
                        }
                        (Value::String(mut left), Value::String(right)) => {
                            left.extend(right.chars());
                            self.stack.push(Value::String(left));
                        }
                        _ => Err(RuntimeError::new(
                            span,
                            "Operands must be two numbers or two strings.",
                        ))?,
                    }
                }
                Instruction::Sub => {
                    let span = instruction.span();
                    let right = self
                        .pop_number(span.clone())
                        .with_message("Operands must be numbers.")?;
                    let left = self
                        .pop_number(span)
                        .with_message("Operands must be numbers.")?;
                    self.stack.push(Value::Number(left - right));
                }
                Instruction::Mul => {
                    let span = instruction.span();
                    let right = self
                        .pop_number(span.clone())
                        .with_message("Operands must be numbers.")?;
                    let left = self
                        .pop_number(span)
                        .with_message("Operands must be numbers.")?;
                    self.stack.push(Value::Number(left * right));
                }
                Instruction::Div => {
                    let span = instruction.span();
                    let right = self
                        .pop_number(span.clone())
                        .with_message("Operands must be numbers.")?;
                    let left = self
                        .pop_number(span)
                        .with_message("Operands must be numbers.")?;
                    self.stack.push(Value::Number(left / right));
                }
                Instruction::Less => {
                    let span = instruction.span();
                    let right = self
                        .pop_number(span.clone())
                        .with_message("Operands must be numbers.")?;
                    let left = self
                        .pop_number(span)
                        .with_message("Operands must be numbers.")?;
                    self.stack.push(Value::Bool(left < right));
                }
                Instruction::LessOrEqual => {
                    let span = instruction.span();
                    let right = self
                        .pop_number(span.clone())
                        .with_message("Operands must be numbers.")?;
                    let left = self
                        .pop_number(span)
                        .with_message("Operands must be numbers.")?;
                    self.stack.push(Value::Bool(left <= right));
                }
                Instruction::Equal => {
                    let span = instruction.span();
                    let right = self.pop_value(span.clone())?;
                    let left = self.pop_value(span)?;
                    self.stack.push(Value::Bool(left == right));
                }
                Instruction::ReadGlobal(name) => {
                    if let Some(value) = self.globals.get(name).clone() {
                        self.stack.push(value.clone());
                    } else {
                        Err(RuntimeError::new(
                            instruction.span(),
                            format!("Undefined variable '{name}'."),
                        ))?;
                    }
                }
                Instruction::WriteGlobal(name) => {
                    let name = name.clone();
                    let value = self.pop_value(instruction.span())?;
                    self.globals.insert(name, value);
                }
            }
            self.pc += 1;
        }

        Ok(())
    }

    fn pop_number(&mut self, span: Span) -> Result<f64, RuntimeError> {
        let value = self.pop_value(span.clone())?;
        match value {
            Value::Number(value) => Ok(value),
            other => Err(RuntimeError::new(
                span,
                format!("Expected a number on the stack, got {other:?}."),
            )),
        }
    }

    fn pop_string(&mut self, span: Span) -> Result<String, RuntimeError> {
        let value = self.pop_value(span.clone())?;
        match value {
            Value::String(value) => Ok(value),
            _ => Err(RuntimeError::new(
                span,
                format!("Expected a string on the stack."),
            )),
        }
    }

    fn pop_boolean(&mut self, span: Span) -> Result<bool, RuntimeError> {
        let value = self.pop_value(span.clone())?;
        Ok(match value {
            Value::Nil => false,
            Value::Bool(value) => value,
            Value::String(_) | Value::Number(_) => true,
        })
    }

    fn pop_value(&mut self, span: Span) -> Result<Value, RuntimeError> {
        let value = self
            .stack
            .pop()
            .ok_or_else(|| RuntimeError::new(span, format!("Expected a value on the stack.")))?;
        Ok(value)
    }
}
