use std::{collections::HashMap, sync::Arc};

use thiserror::Error;

use crate::{ast::Spanned, lexer::Span};

#[derive(Debug, PartialEq, Clone, Copy)]
pub(crate) enum JumpCondition {
    Always,
    Truthy,
    Falsy,
}

#[derive(Debug, PartialEq)]
pub(crate) enum Instruction {
    // +1
    Dup,
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
    // 0
    WriteGlobal(String),
    // +1
    ReadStackAbsolute(usize),
    // 0
    WriteStackAbsolute(usize),
    // -1 (if condition is not Always)
    Jump {
        condition: JumpCondition,
        target: usize,
    },
    // -(arity+1), +1
    Call {
        arity: usize,
    },
}

#[derive(Debug, PartialEq)]
pub struct Chunk {
    pub(crate) instructions: Vec<Spanned<Instruction>>,
}

#[derive(Debug, PartialEq)]
pub struct Program {
    main_chunk: Arc<Chunk>,
    chunks: Vec<Arc<Chunk>>,
}
impl Program {
    pub fn new(main_chunk: Chunk) -> Self {
        Self {
            main_chunk: Arc::new(main_chunk),
            chunks: Vec::new(),
        }
    }

    pub fn add_chunk(&mut self, chunk: Chunk) -> usize {
        self.chunks.push(Arc::new(chunk));
        self.chunks.len() - 1
    }
}

#[derive(Clone)]
struct NativeFn(Arc<dyn Fn(&[Value]) -> Result<Value, RuntimeError>>);
impl std::fmt::Debug for NativeFn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<native fn>")
    }
}
impl PartialEq for NativeFn {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.0, &other.0)
    }
}

#[derive(Debug, PartialEq, Clone)]
enum Value {
    Nil,
    Bool(bool),
    String(String),
    Number(f64),
    Script {
        chunk: Arc<Chunk>,
    },
    Function {
        chunk: Arc<Chunk>,
        arity: usize,
        name: String,
    },
    NativeFunction {
        arity: usize,
        function: NativeFn,
    },
}
impl Value {
    fn truthy(&self) -> bool {
        match self {
            Value::Nil => false,
            Value::Bool(value) => *value,
            Value::String(_) | Value::Number(_) => true,
            Value::Script { .. } => true,
            Value::Function { .. } => true,
            Value::NativeFunction { .. } => true,
        }
    }
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

struct StackFrame {
    pc: usize,
    bottom_of_stack: usize,
}

struct Stack {
    data: Vec<Value>,
    frames: Vec<StackFrame>,
}
impl Stack {
    fn new() -> Self {
        Self {
            data: Vec::new(),
            frames: Vec::new(),
        }
    }

    fn clear(&mut self) {
        self.data.clear();
        self.frames.clear();
    }

    fn push(&mut self, value: Value) {
        self.data.push(value);
    }

    fn pop(&mut self) -> Option<Value> {
        (!self.is_empty()).then(|| self.data.pop()).flatten()
    }

    fn get(&self, index: usize) -> Option<&Value> {
        self.data.get(index + self.bottom_of_stack())
    }

    fn get_mut(&mut self, index: usize) -> Option<&mut Value> {
        let bottom_of_stack = self.bottom_of_stack();
        self.data.get_mut(index + bottom_of_stack)
    }

    fn current_chunk(&self) -> Option<&Chunk> {
        self.data.get(self.bottom_of_stack()).and_then(|v| match v {
            Value::Script { chunk } => Some(chunk.as_ref()),
            Value::Function { chunk, .. } => Some(chunk.as_ref()),
            _ => None,
        })
    }

    fn pc(&self) -> usize {
        self.frames.last().map(|frame| frame.pc).unwrap_or(0)
    }

    fn set_pc(&mut self, pc: usize) {
        self.frames
            .last_mut()
            .expect("At least one frame must be present")
            .pc = pc;
    }

    fn slice(&self) -> &[Value] {
        &self.data[self.bottom_of_stack()..]
    }

    /// Pops the last frame from the stack.
    /// Leaves the first value in the frame on top of the stack.
    fn pop_frame(&mut self) {
        self.data.truncate(self.bottom_of_stack() + 1);
        self.frames.pop();
    }

    /// Pushes a new frame onto the stack.
    /// It will contain the top `size + 1` values on the stack.
    fn push_frame(&mut self, size: usize) {
        assert!(self.data.len() >= size + 1, "Stack is too small");
        self.frames.push(StackFrame {
            pc: 0,
            bottom_of_stack: self.data.len() - size - 1,
        });
    }

    fn peek(&self) -> Option<&Value> {
        (!self.is_empty()).then(|| self.data.last()).flatten()
    }

    fn is_empty(&self) -> bool {
        self.data.len() <= self.bottom_of_stack()
    }

    fn bottom_of_stack(&self) -> usize {
        self.frames
            .last()
            .map(|frame| frame.bottom_of_stack)
            .unwrap_or(0)
    }
}

pub struct Vm<'env> {
    env: &'env mut dyn ExecutionEnv,
    globals: HashMap<String, Value>,
    stack: Stack,
    main_chunk: Arc<Chunk>,
}
impl<'env> Vm<'env> {
    pub fn new(env: &'env mut dyn ExecutionEnv, program: Program) -> Self {
        Self {
            env,
            main_chunk: program.main_chunk.clone(),
            stack: Stack::new(),
            globals: HashMap::new(),
        }
    }

    pub fn reset(&mut self) {
        self.stack.clear();
        self.globals.clear();

        self.stack.push(Value::Script {
            chunk: self.main_chunk.clone(),
        });
        self.stack.push_frame(0);

        self.globals.insert(
            "clock".into(),
            Value::NativeFunction {
                arity: 0,
                function: (NativeFn(Arc::new(|_| {
                    Ok(Value::Number(
                        std::time::SystemTime::now()
                            .duration_since(std::time::UNIX_EPOCH)
                            .unwrap()
                            .as_secs_f64(),
                    ))
                }))),
            },
        );
    }

    fn push_value(&mut self, value: Value) {
        self.stack.push(value);
    }

    pub fn run(&mut self) -> Result<(), RuntimeError> {
        while let Some(instruction) = self
            .stack
            .current_chunk()
            .and_then(|chunk| chunk.instructions.get(self.stack.pc()))
        {
            //eprintln!(
            //    "PC = {}, Instruction = {:?}, TOS = {:?}",
            //    self.stack.pc(),
            //    instruction.value(),
            //    self.peek_value(instruction.span())
            //);
            match instruction.value() {
                Instruction::Dup => {
                    let value = self.peek_value(instruction.span())?;
                    self.push_value(value.clone());
                }
                Instruction::PushNil => {
                    self.push_value(Value::Nil);
                }
                Instruction::PushBool(value) => {
                    self.push_value(Value::Bool(*value));
                }
                Instruction::PushString(value) => {
                    self.push_value(Value::String(value.clone()));
                }
                Instruction::PushNumber(value) => {
                    self.push_value(Value::Number(*value));
                }
                Instruction::Print => match self.peek_value(instruction.span())? {
                    Value::String(value) => self.env.print(&value.clone()),
                    Value::Number(value) => self.env.print(&format!("{value}")),
                    Value::Nil => self.env.print("nil"),
                    Value::Bool(true) => self.env.print("true"),
                    Value::Bool(false) => self.env.print("false"),
                    Value::Script { .. } => self.env.print("<script>"),
                    Value::Function { name, .. } => self.env.print(&format!("<fn {name}>")),
                    Value::NativeFunction { .. } => self.env.print("<native fn>"),
                },
                Instruction::Pop => {
                    self.pop_value(instruction.span())?;
                }
                Instruction::Negate => {
                    let value = self
                        .pop_number(instruction.span())
                        .with_message("Operand must be a number.")?;
                    self.push_value(Value::Number(-value));
                }
                Instruction::LogicalNot => {
                    let value = self.pop_boolean(instruction.span())?;
                    self.push_value(Value::Bool(!value));
                }
                Instruction::Add => {
                    let span = instruction.span();
                    let right = self.pop_value(span.clone())?;
                    let left = self.pop_value(span.clone())?;
                    match (left, right) {
                        (Value::Number(left), Value::Number(right)) => {
                            self.push_value(Value::Number(left + right));
                        }
                        (Value::String(mut left), Value::String(right)) => {
                            left.extend(right.chars());
                            self.push_value(Value::String(left));
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
                    self.push_value(Value::Number(left - right));
                }
                Instruction::Mul => {
                    let span = instruction.span();
                    let right = self
                        .pop_number(span.clone())
                        .with_message("Operands must be numbers.")?;
                    let left = self
                        .pop_number(span)
                        .with_message("Operands must be numbers.")?;
                    self.push_value(Value::Number(left * right));
                }
                Instruction::Div => {
                    let span = instruction.span();
                    let right = self
                        .pop_number(span.clone())
                        .with_message("Operands must be numbers.")?;
                    let left = self
                        .pop_number(span)
                        .with_message("Operands must be numbers.")?;
                    self.push_value(Value::Number(left / right));
                }
                Instruction::Less => {
                    let span = instruction.span();
                    let right = self
                        .pop_number(span.clone())
                        .with_message("Operands must be numbers.")?;
                    let left = self
                        .pop_number(span)
                        .with_message("Operands must be numbers.")?;
                    self.push_value(Value::Bool(left < right));
                }
                Instruction::LessOrEqual => {
                    let span = instruction.span();
                    let right = self
                        .pop_number(span.clone())
                        .with_message("Operands must be numbers.")?;
                    let left = self
                        .pop_number(span)
                        .with_message("Operands must be numbers.")?;
                    self.push_value(Value::Bool(left <= right));
                }
                Instruction::Equal => {
                    let span = instruction.span();
                    let right = self.pop_value(span.clone())?;
                    let left = self.pop_value(span)?;
                    self.push_value(Value::Bool(left == right));
                }
                Instruction::ReadGlobal(name) => {
                    if let Some(value) = self.globals.get(name).clone() {
                        self.push_value(value.clone());
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
                    self.globals.insert(name, value.clone());
                    self.push_value(value);
                }
                Instruction::ReadStackAbsolute(index) => {
                    self.push_value(
                        self.stack
                            .get(*index)
                            .cloned()
                            .ok_or_else(|| {
                                RuntimeError::new(
                                    instruction.span(),
                                    format!("Index out of bounds."),
                                )
                            })?
                            .clone(),
                    );
                }
                Instruction::WriteStackAbsolute(index) => {
                    let index = *index;
                    let span = instruction.span();
                    let value = self.peek_value(span.clone())?;
                    *self.stack.get_mut(index).ok_or_else(|| {
                        RuntimeError::new(span, format!("Index out of bounds."))
                    })? = value.clone();
                }
                Instruction::Jump { condition, target } => {
                    let target = *target;
                    let should_jump = match condition {
                        JumpCondition::Always => true,
                        JumpCondition::Truthy => self.pop_boolean(instruction.span())?,
                        JumpCondition::Falsy => !self.pop_boolean(instruction.span())?,
                    };
                    if should_jump {
                        self.stack.set_pc(target);
                        continue;
                    }
                }
                Instruction::Call { arity } => {
                    let span = instruction.span();
                    let arity = *arity;
                    self.stack.push_frame(arity);
                    match self.peek_value_at(0, span.clone())? {
                        Value::Function {
                            chunk,
                            arity: fun_arity,
                            name,
                        } => {
                            if *fun_arity != arity {
                                Err(RuntimeError::new(
                                    span,
                                    "Expected a function with the given arity.",
                                ))?;
                            }
                            continue;
                        }
                        Value::NativeFunction {
                            arity: fun_arity,
                            function,
                        } => {
                            if *fun_arity != arity {
                                Err(RuntimeError::new(
                                    span,
                                    "Expected a function with the given arity.",
                                ))?;
                            }
                            let function = function.clone();
                            let result = function.0(&self.stack.slice()[1..])?;
                            *self.stack.get_mut(0).expect("Frame always has slot 0") = result;
                            self.stack.pop_frame();
                        }
                        _ => Err(RuntimeError::new(span, "Expected a function on the stack."))?,
                    }
                }
            }
            self.stack.set_pc(self.stack.pc() + 1);
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
        Ok(value.truthy())
    }

    fn pop_value(&mut self, span: Span) -> Result<Value, RuntimeError> {
        let value = self
            .stack
            .pop()
            .ok_or_else(|| RuntimeError::new(span, format!("Expected a value on the stack.")))?;
        Ok(value)
    }

    fn peek_value(&self, span: Span) -> Result<&Value, RuntimeError> {
        self.stack
            .peek()
            .ok_or_else(|| RuntimeError::new(span, format!("Expected a value on the stack.")))
    }

    fn peek_value_at(&self, offset: usize, span: Span) -> Result<&Value, RuntimeError> {
        self.stack
            .get(offset)
            .ok_or_else(|| RuntimeError::new(span, format!("Expected a value on the stack.")))
    }
}
