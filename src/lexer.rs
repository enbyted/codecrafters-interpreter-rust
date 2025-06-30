use logos::Logos;
use std::borrow::Cow;

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
pub enum TokenValue {
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
    type Item = Result<Token<'inp>, ()>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.inner.next() {
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
