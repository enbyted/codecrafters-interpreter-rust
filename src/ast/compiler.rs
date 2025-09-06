use crate::{ast::Spanned, lexer::Span, vm};

pub(super) struct Compiler {
    instructions: Vec<Spanned<vm::Instruction>>,
}
impl Compiler {
    pub(super) fn new() -> Self {
        Self {
            instructions: Vec::new(),
        }
    }

    pub(super) fn finalize(self) -> vm::Program {
        vm::Program {
            instructions: self.instructions,
        }
    }

    pub(super) fn push(&mut self, span: Span, instruction: vm::Instruction) {
        self.instructions.push(Spanned::new(span, instruction));
    }

    /// Reads value of the given variable by name and puts in on the top of the stack
    pub(super) fn read_variable(&mut self, name: Spanned<&str>) {
        // TODO: Handle scopes
        self.instructions.push(Spanned::new(
            name.span(),
            vm::Instruction::ReadGlobal(name.value.to_string()),
        ));
    }

    /// Declares a new variable in the current scope
    pub(super) fn declare_variable(&mut self, name: Spanned<&str>) {
        // TODO: Handle scopes
    }

    /// Writes the value on the top of the stack to the given variable by name
    /// Consumes the top of the stack
    pub(super) fn write_variable(&mut self, name: Spanned<&str>) {
        // TODO: Handle scopes
        self.instructions.push(Spanned::new(
            name.span(),
            vm::Instruction::WriteGlobal(name.value.to_string()),
        ));
    }
}
