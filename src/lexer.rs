use logos::{Logos, Skip};
use std::borrow::Cow;

#[derive(Debug, Clone, PartialEq, Default)]
pub enum LexerErrorKind {
    UnexpectedCharacter(char),
    #[default]
    Other,
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct LexerError {
    kind: LexerErrorKind,
    line: Option<usize>,
}
impl LexerError {
    fn from_lexer(lex: &mut logos::Lexer<'_, TokenValue>) -> Self {
        let character = lex.slice().chars().next().unwrap();
        Self {
            kind: LexerErrorKind::UnexpectedCharacter(character),
            line: Some(lex.extras.line),
        }
    }

    pub fn line(&self) -> Option<usize> {
        self.line
    }

    pub fn message(&self) -> String {
        match self.kind {
            LexerErrorKind::UnexpectedCharacter(character) => {
                format!("Unexpected character: {character}")
            }
            LexerErrorKind::Other => format!("Unexpected error"),
        }
    }

    pub fn cc_format(&self) -> String {
        let prefix = if let Some(line) = self.line {
            format!("[line {line}] ")
        } else {
            String::new()
        };
        format!("{prefix}Error: {}", self.message())
    }
}

pub struct LexerExtras {
    line: usize,
}
impl Default for LexerExtras {
    fn default() -> Self {
        Self { line: 1 }
    }
}

#[derive(Debug)]
pub enum TokenPayload {
    Null,
}
impl TokenPayload {
    pub fn diag_value(&self) -> Cow<'_, str> {
        match self {
            TokenPayload::Null => "null".into(),
        }
    }
}

#[derive(Debug, Logos)]
#[logos(error = LexerError)]
#[logos(extras = LexerExtras)]
#[logos(skip r"[ \t\f]+")]
pub enum TokenValue {
    #[regex(r"\n", newline_callback)]
    Newline,

    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token("*")]
    Star,
    #[token(".")]
    Dot,
    #[token(",")]
    Comma,
    #[token(";")]
    Semicolon,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("/")]
    Slash,
    Eof,
}
impl TokenValue {
    pub fn diag_name(&self) -> &'static str {
        match self {
            TokenValue::Newline => unreachable!(),
            TokenValue::LParen => "LEFT_PAREN",
            TokenValue::RParen => "RIGHT_PAREN",
            TokenValue::LBrace => "LEFT_BRACE",
            TokenValue::RBrace => "RIGHT_BRACE",
            TokenValue::Star => "STAR",
            TokenValue::Dot => "DOT",
            TokenValue::Comma => "COMMA",
            TokenValue::Semicolon => "SEMICOLON",
            TokenValue::Plus => "PLUS",
            TokenValue::Minus => "MINUS",
            TokenValue::Slash => "SLASH",
            TokenValue::Eof => "EOF",
        }
    }

    pub fn payload(&self) -> TokenPayload {
        match self {
            TokenValue::Newline => unreachable!(),
            TokenValue::LParen
            | TokenValue::RParen
            | TokenValue::Eof
            | TokenValue::LBrace
            | TokenValue::RBrace
            | TokenValue::Star
            | TokenValue::Dot
            | TokenValue::Comma
            | TokenValue::Semicolon
            | TokenValue::Plus
            | TokenValue::Minus
            | TokenValue::Slash => TokenPayload::Null,
        }
    }
}

fn newline_callback(lex: &mut logos::Lexer<'_, TokenValue>) -> Skip {
    lex.extras.line += 1;
    Skip
}

pub struct Token<'inp> {
    value: TokenValue,
    lexeme: &'inp str,
}
impl<'inp> Token<'inp> {
    pub fn value(&self) -> &TokenValue {
        &self.value
    }

    pub fn into_value(self) -> TokenValue {
        self.value
    }

    pub fn lexeme(&self) -> &'inp str {
        self.lexeme
    }
}

pub struct Lexer<'inp> {
    inner: logos::Lexer<'inp, TokenValue>,
    emitted_eof: bool,
}
impl<'inp> Lexer<'inp> {
    pub fn new(source: &'inp str) -> Self {
        Self {
            inner: TokenValue::lexer(source),
            emitted_eof: false,
        }
    }
}
impl<'inp> Iterator for Lexer<'inp> {
    type Item = Result<Token<'inp>, LexerError>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.inner.next() {
            Some(Err(LexerError {
                kind: LexerErrorKind::Other,
                ..
            })) => Some(Err(LexerError::from_lexer(&mut self.inner))),
            Some(value) => Some(value.map(|value| Token {
                value,
                lexeme: self.inner.slice(),
            })),
            None if !self.emitted_eof => {
                self.emitted_eof = true;
                Some(Ok(Token {
                    value: TokenValue::Eof,
                    lexeme: "",
                }))
            }
            None => None,
        }
    }
}

#[cfg(test)]
mod test;
