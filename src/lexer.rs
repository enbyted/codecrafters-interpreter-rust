use logos::{Logos, Skip};
use std::borrow::Cow;

#[derive(Debug, Clone, PartialEq, Default)]
pub enum LexerErrorKind {
    UnexpectedCharacter(char),
    UnterminatedString,
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

    fn unterminated_string(lex: &logos::Lexer<'_, TokenValue>) -> Self {
        Self {
            kind: LexerErrorKind::UnterminatedString,
            line: Some(lex.extras.line),
        }
    }

    fn from_parse_float(
        lex: &logos::Lexer<'_, TokenValue>,
        error: std::num::ParseFloatError,
    ) -> Self {
        todo!()
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
            LexerErrorKind::UnterminatedString => format!("Unterminated string."),
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
    String(String),
    Number(f64),
}
impl TokenPayload {
    pub fn diag_value(&self) -> Cow<'_, str> {
        match self {
            TokenPayload::Null => "null".into(),
            TokenPayload::String(v) => Cow::Borrowed(v),
            TokenPayload::Number(v) => format!("{v:?}").into(),
        }
    }
}

#[derive(Debug, Logos)]
#[logos(error = LexerError)]
#[logos(extras = LexerExtras)]
#[logos(skip r"[ \t\f]+")]
#[logos(skip r"//[^\n]*")]
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

    #[token("=")]
    Equal,
    #[token("==")]
    EqualEqual,
    #[token("!")]
    Bang,
    #[token("!=")]
    BangEqual,
    #[token("<")]
    Less,
    #[token("<=")]
    LessEqual,
    #[token(">")]
    Greater,
    #[token(">=")]
    GreaterEqual,

    #[regex(r#""[^"]*""#, |v| Some(String::from(&v.slice()[1..v.slice().len()-1])))]
    #[regex(r#""[^"]*"#, |v| Err(LexerError::unterminated_string(v)))]
    String(String),

    #[regex(r"[0-9]+", |v| v.slice().parse().map_err(|e| LexerError::from_parse_float(v, e)))]
    #[regex(r"[0-9]+\.[0-9]+", |v| v.slice().parse().map_err(|e| LexerError::from_parse_float(v, e)))]
    Number(f64),

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
            TokenValue::Equal => "EQUAL",
            TokenValue::EqualEqual => "EQUAL_EQUAL",
            TokenValue::Bang => "BANG",
            TokenValue::BangEqual => "BANG_EQUAL",
            TokenValue::Less => "LESS",
            TokenValue::LessEqual => "LESS_EQUAL",
            TokenValue::Greater => "GREATER",
            TokenValue::GreaterEqual => "GREATER_EQUAL",
            TokenValue::String(_) => "STRING",
            TokenValue::Number(_) => "NUMBER",
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
            | TokenValue::Equal
            | TokenValue::EqualEqual
            | TokenValue::Bang
            | TokenValue::BangEqual
            | TokenValue::Less
            | TokenValue::LessEqual
            | TokenValue::Greater
            | TokenValue::GreaterEqual
            | TokenValue::Slash => TokenPayload::Null,
            TokenValue::String(v) => TokenPayload::String(v.clone()),
            TokenValue::Number(v) => TokenPayload::Number(*v),
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
