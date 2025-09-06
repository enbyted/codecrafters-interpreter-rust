use std::collections::HashMap;

use crate::{ast::Spanned, lexer::Span, vm};

pub(super) enum ScopeType {
    Block,
}

struct Scope {
    kind: ScopeType,
    start_of_frame: usize,
    locals: HashMap<String, usize>,
}
impl Scope {
    fn new(kind: ScopeType, start_of_frame: usize) -> Self {
        Self {
            kind,
            start_of_frame,
            locals: HashMap::new(),
        }
    }

    fn end_of_frame(&self) -> usize {
        self.start_of_frame + self.locals.len()
    }

    fn frame_size(&self) -> usize {
        self.locals.len()
    }

    fn add_local(&mut self, name: String) -> bool {
        use std::collections::hash_map::Entry;
        let index = self.locals.len();
        match self.locals.entry(name) {
            Entry::Occupied(_entry) => false,
            Entry::Vacant(entry) => {
                entry.insert(index);
                true
            }
        }
    }

    fn resolve_local(&self, name: &str) -> Option<usize> {
        self.locals
            .get(name)
            .copied()
            .map(|index| index + self.start_of_frame)
    }
}

pub(super) struct Compiler {
    instructions: Vec<Spanned<vm::Instruction>>,
    scope: Vec<Scope>,
}
impl Compiler {
    pub(super) fn new() -> Self {
        Self {
            instructions: Vec::new(),
            scope: Vec::new(),
        }
    }

    pub(super) fn enter_scope(&mut self, scope_type: ScopeType) {
        let start_of_frame = if let Some(scope) = self.scope.last() {
            scope.end_of_frame()
        } else {
            0
        };
        self.scope.push(Scope::new(scope_type, start_of_frame));
    }

    pub(super) fn exit_scope(&mut self) {
        if let Some(scope) = self.scope.pop() {
            for _ in 0..scope.frame_size() {
                self.instructions.push(Spanned::new(
                    Span {
                        range: 0..0,
                        line_start: 0,
                    },
                    vm::Instruction::Pop,
                ));
            }
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
        if let Some(index) = self.find_var_in_scope(name.value()) {
            self.instructions.push(Spanned::new(
                name.span(),
                vm::Instruction::ReadStackAbsolute(index),
            ));
        } else {
            self.instructions.push(Spanned::new(
                name.span(),
                vm::Instruction::ReadGlobal(name.value.to_string()),
            ));
        }
    }

    /// Declares a new variable in the current scope
    ///
    /// This cannot be used in the middle of an expression.
    pub(super) fn declare_variable(&mut self, name: Spanned<&str>) {
        if let Some(scope) = self.scope.last_mut() {
            if scope.add_local(name.value.to_string()) {
                self.push(name.span(), vm::Instruction::PushNil);
            }
        }
    }

    /// Writes the value on the top of the stack to the given variable by name
    pub(super) fn write_variable(&mut self, name: Spanned<&str>) {
        if let Some(index) = self.find_var_in_scope(name.value()) {
            self.instructions.push(Spanned::new(
                name.span(),
                vm::Instruction::WriteStackAbsolute(index),
            ));
        } else {
            // TODO: This will also create a global variable which is not okay
            //       That should happen as part of the declaration
            self.instructions.push(Spanned::new(
                name.span(),
                vm::Instruction::WriteGlobal(name.value.to_string()),
            ));
        }
    }

    fn find_var_in_scope(&self, name: &str) -> Option<usize> {
        for scope in self.scope.iter().rev() {
            if let Some(index) = scope.resolve_local(name) {
                return Some(index);
            }
        }
        None
    }
}
