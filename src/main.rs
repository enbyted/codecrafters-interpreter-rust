use std::env;
use std::fs;
use std::io::{self, Write};

use codecrafters_interpreter::ast;
use codecrafters_interpreter::lexer::Lexer;
use codecrafters_interpreter::vm::ExecutionEnv;
use codecrafters_interpreter::vm::Vm;

enum ResultCode {
    Ok,
    HasLexingErrors,
    ExecutionError,
}
impl std::process::Termination for ResultCode {
    fn report(self) -> std::process::ExitCode {
        match self {
            ResultCode::Ok => 0.into(),
            ResultCode::HasLexingErrors => 65.into(),
            ResultCode::ExecutionError => 70.into(),
        }
    }
}

fn main() -> ResultCode {
    let args: Vec<String> = env::args().collect();
    if args.len() < 3 {
        writeln!(io::stderr(), "Usage: {} tokenize <filename>", args[0]).unwrap();
        return ResultCode::Ok;
    }

    let command = &args[1];
    let filename = &args[2];

    match command.as_str() {
        "tokenize" => {
            // You can use print statements as follows for debugging, they'll be visible when running tests.
            writeln!(io::stderr(), "Logs from your program will appear here!").unwrap();

            let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
                writeln!(io::stderr(), "Failed to read file {}", filename).unwrap();
                String::new()
            });

            let mut errors = Vec::new();
            let mut tokens = Vec::new();

            for token in Lexer::new(&file_contents) {
                match token {
                    Ok(token) => tokens.push(token),
                    Err(error) => errors.push(error),
                }
            }

            for error in &errors {
                eprintln!("{}", error.cc_format());
            }

            for token in tokens {
                println!(
                    "{} {} {}",
                    token.value().diag_name(),
                    token.lexeme(),
                    token.value().payload().diag_value()
                );
            }

            if errors.is_empty() {
                ResultCode::Ok
            } else {
                ResultCode::HasLexingErrors
            }
        }
        "parse" => {
            let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
                writeln!(io::stderr(), "Failed to read file {}", filename).unwrap();
                String::new()
            });

            let mut errors = Vec::new();
            let program = ast::parse(Lexer::new(&file_contents), &mut errors);
            for error in &errors {
                eprintln!("{error}");
            }
            for expr in program.exprs {
                expr.value().diag_print(&mut std::io::stdout()).unwrap();
                println!();
            }

            if errors.is_empty() {
                ResultCode::Ok
            } else {
                ResultCode::HasLexingErrors
            }
        }
        "evaluate" => {
            let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
                writeln!(io::stderr(), "Failed to read file {}", filename).unwrap();
                String::new()
            });

            let mut errors = Vec::new();
            let program = ast::parse(Lexer::new(&file_contents), &mut errors);
            for error in &errors {
                eprintln!("{error}");
            }

            if errors.is_empty() {
                let program = program.compile();
                struct DummyEnv;
                impl ExecutionEnv for DummyEnv {
                    fn print(&mut self, value: &str) {
                        println!("{value}");
                    }
                }
                let mut env = DummyEnv;
                let mut vm = Vm::new(&mut env, program);
                if let Err(e) = vm.run() {
                    eprintln!("{e}");
                    return ResultCode::ExecutionError;
                }
            }

            if errors.is_empty() {
                ResultCode::Ok
            } else {
                ResultCode::HasLexingErrors
            }
        }
        _ => {
            writeln!(io::stderr(), "Unknown command: {}", command).unwrap();
            return ResultCode::Ok;
        }
    }
}
