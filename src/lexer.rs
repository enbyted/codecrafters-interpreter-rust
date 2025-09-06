use logos::{Logos, Skip};
use std::{borrow::Cow, ops::Range};

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
    span: Option<Span>,
}
impl LexerError {
    fn from_lexer(lex: &logos::Lexer<'_, TokenValue>) -> Self {
        let character = lex.slice().chars().next().unwrap();
        Self {
            kind: LexerErrorKind::UnexpectedCharacter(character),
            span: Some(Span::from(lex)),
        }
    }

    fn unterminated_string(lex: &logos::Lexer<'_, TokenValue>) -> Self {
        Self {
            kind: LexerErrorKind::UnterminatedString,
            span: Some(Span::from(lex)),
        }
    }

    fn from_parse_float(
        lex: &logos::Lexer<'_, TokenValue>,
        error: std::num::ParseFloatError,
    ) -> Self {
        todo!()
    }

    pub fn line(&self) -> Option<usize> {
        self.span.as_ref().map(|s| s.line_start)
    }

    pub fn range(&self) -> Option<(usize, usize)> {
        self.span.as_ref().map(|s| (s.start(), s.end()))
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
        let prefix = if let Some(line) = self.line() {
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum TokenKind {
    LParen,
    RParen,
    LBrace,
    RBrace,
    Star,
    Dot,
    Comma,
    Semicolon,
    Plus,
    Minus,
    Slash,
    Equal,
    EqualEqual,
    Bang,
    BangEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    KwAnd,
    KwClass,
    KwElse,
    KwFalse,
    KwFor,
    KwFun,
    KwIf,
    KwNil,
    KwOr,
    KwPrint,
    KwReturn,
    KwSuper,
    KwThis,
    KwTrue,
    KwVar,
    KwWhile,
    Identifier,
}
impl TokenKind {
    pub(super) fn symbol(&self) -> &'static str {
        match self {
            TokenKind::LParen => "(",
            TokenKind::RParen => ")",
            TokenKind::LBrace => "{",
            TokenKind::RBrace => "}",
            TokenKind::Star => "*",
            TokenKind::Dot => ".",
            TokenKind::Comma => ",",
            TokenKind::Semicolon => ";",
            TokenKind::Plus => "+",
            TokenKind::Minus => "-",
            TokenKind::Slash => "/",
            TokenKind::Equal => "=",
            TokenKind::EqualEqual => "==",
            TokenKind::Bang => "!",
            TokenKind::BangEqual => "!=",
            TokenKind::Less => "<",
            TokenKind::LessEqual => "<=",
            TokenKind::Greater => ">",
            TokenKind::GreaterEqual => ">=",
            TokenKind::KwAnd => "and",
            TokenKind::KwClass => "class",
            TokenKind::KwElse => "else",
            TokenKind::KwFalse => "false",
            TokenKind::KwFor => "for",
            TokenKind::KwFun => "fun",
            TokenKind::KwIf => "if",
            TokenKind::KwNil => "nil",
            TokenKind::KwOr => "or",
            TokenKind::KwPrint => "print",
            TokenKind::KwReturn => "return",
            TokenKind::KwSuper => "super",
            TokenKind::KwThis => "this",
            TokenKind::KwTrue => "true",
            TokenKind::KwVar => "var",
            TokenKind::KwWhile => "while",
            TokenKind::Identifier => "IDENTIFIER",
        }
    }
}
impl PartialEq<&TokenValue> for TokenKind {
    fn eq(&self, other: &&TokenValue) -> bool {
        match (self, other) {
            (Self::LParen, TokenValue::LParen) => true,
            (Self::RParen, TokenValue::RParen) => true,
            (TokenKind::LBrace, TokenValue::LBrace) => true,
            (TokenKind::RBrace, TokenValue::RBrace) => true,
            (TokenKind::Star, TokenValue::Star) => true,
            (TokenKind::Dot, TokenValue::Dot) => true,
            (TokenKind::Comma, TokenValue::Comma) => true,
            (TokenKind::Semicolon, TokenValue::Semicolon) => true,
            (TokenKind::Plus, TokenValue::Plus) => true,
            (TokenKind::Minus, TokenValue::Minus) => true,
            (TokenKind::Slash, TokenValue::Slash) => true,
            (TokenKind::Equal, TokenValue::Equal) => true,
            (TokenKind::EqualEqual, TokenValue::EqualEqual) => true,
            (TokenKind::Bang, TokenValue::Bang) => true,
            (TokenKind::BangEqual, TokenValue::BangEqual) => true,
            (TokenKind::Less, TokenValue::Less) => true,
            (TokenKind::LessEqual, TokenValue::LessEqual) => true,
            (TokenKind::Greater, TokenValue::Greater) => true,
            (TokenKind::GreaterEqual, TokenValue::GreaterEqual) => true,
            (TokenKind::KwAnd, TokenValue::KwAnd) => true,
            (TokenKind::KwClass, TokenValue::KwClass) => true,
            (TokenKind::KwElse, TokenValue::KwElse) => true,
            (TokenKind::KwFalse, TokenValue::KwFalse) => true,
            (TokenKind::KwFor, TokenValue::KwFor) => true,
            (TokenKind::KwFun, TokenValue::KwFun) => true,
            (TokenKind::KwIf, TokenValue::KwIf) => true,
            (TokenKind::KwNil, TokenValue::KwNil) => true,
            (TokenKind::KwOr, TokenValue::KwOr) => true,
            (TokenKind::KwPrint, TokenValue::KwPrint) => true,
            (TokenKind::KwReturn, TokenValue::KwReturn) => true,
            (TokenKind::KwSuper, TokenValue::KwSuper) => true,
            (TokenKind::KwThis, TokenValue::KwThis) => true,
            (TokenKind::KwTrue, TokenValue::KwTrue) => true,
            (TokenKind::KwVar, TokenValue::KwVar) => true,
            (TokenKind::KwWhile, TokenValue::KwWhile) => true,
            (TokenKind::Identifier, TokenValue::Identifier(_)) => true,
            _ => false,
        }
    }
}
impl PartialEq<TokenValue> for TokenKind {
    fn eq(&self, other: &TokenValue) -> bool {
        self == &other
    }
}
impl PartialEq<TokenKind> for TokenValue {
    fn eq(&self, other: &TokenKind) -> bool {
        other.eq(self)
    }
}
impl PartialEq<TokenKind> for &TokenValue {
    fn eq(&self, other: &TokenKind) -> bool {
        other.eq(self)
    }
}
#[derive(Debug, Logos, Clone, PartialEq)]
#[logos(error = LexerError)]
#[logos(extras = LexerExtras)]
#[logos(skip r"[ \t\f]+")]
#[logos(skip r"//[^\n]*")]
pub enum TokenValue {
    #[regex(r"\n", newline_callback)]
    Newline,
    Error(LexerError),

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

    #[token("and")]
    KwAnd,
    #[token("class")]
    KwClass,
    #[token("else")]
    KwElse,
    #[token("false")]
    KwFalse,
    #[token("for")]
    KwFor,
    #[token("fun")]
    KwFun,
    #[token("if")]
    KwIf,
    #[token("nil")]
    KwNil,
    #[token("or")]
    KwOr,
    #[token("print")]
    KwPrint,
    #[token("return")]
    KwReturn,
    #[token("super")]
    KwSuper,
    #[token("this")]
    KwThis,
    #[token("true")]
    KwTrue,
    #[token("var")]
    KwVar,
    #[token("while")]
    KwWhile,

    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", |v| String::from(v.slice()))]
    Identifier(String),

    Eof,
}
impl TokenValue {
    pub fn diag_name(&self) -> &'static str {
        match self {
            TokenValue::Newline => unreachable!(),
            TokenValue::Error(_) => unreachable!(),
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
            TokenValue::KwAnd => "AND",
            TokenValue::KwClass => "CLASS",
            TokenValue::KwElse => "ELSE",
            TokenValue::KwFalse => "FALSE",
            TokenValue::KwFor => "FOR",
            TokenValue::KwFun => "FUN",
            TokenValue::KwIf => "IF",
            TokenValue::KwNil => "NIL",
            TokenValue::KwOr => "OR",
            TokenValue::KwPrint => "PRINT",
            TokenValue::KwReturn => "RETURN",
            TokenValue::KwSuper => "SUPER",
            TokenValue::KwThis => "THIS",
            TokenValue::KwTrue => "TRUE",
            TokenValue::KwVar => "VAR",
            TokenValue::KwWhile => "WHILE",
            TokenValue::Identifier(_) => "IDENTIFIER",
        }
    }

    pub fn payload(&self) -> TokenPayload {
        match self {
            TokenValue::Newline => unreachable!(),
            TokenValue::Error(_) => unreachable!(),
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
            | TokenValue::Slash
            | TokenValue::KwClass
            | TokenValue::KwElse
            | TokenValue::KwFalse
            | TokenValue::KwFor
            | TokenValue::KwFun
            | TokenValue::KwIf
            | TokenValue::KwNil
            | TokenValue::KwOr
            | TokenValue::KwPrint
            | TokenValue::KwReturn
            | TokenValue::KwSuper
            | TokenValue::KwThis
            | TokenValue::KwTrue
            | TokenValue::KwVar
            | TokenValue::KwWhile
            | TokenValue::Identifier(_)
            | TokenValue::KwAnd => TokenPayload::Null,
            TokenValue::String(v) => TokenPayload::String(v.clone()),
            TokenValue::Number(v) => TokenPayload::Number(*v),
        }
    }
}

fn newline_callback(lex: &mut logos::Lexer<'_, TokenValue>) -> Skip {
    lex.extras.line += 1;
    Skip
}

#[derive(Debug, Clone, PartialEq)]
pub struct Span {
    pub(super) range: Range<usize>,
    pub(super) line_start: usize,
}
impl Span {
    #[cfg(test)]
    pub(crate) const fn empty() -> Self {
        Self {
            range: 0..0,
            line_start: 0,
        }
    }

    pub fn start(&self) -> usize {
        self.range.start
    }

    pub fn end(&self) -> usize {
        self.range.end
    }

    pub fn start_line(&self) -> usize {
        self.line_start
    }
}
impl From<&logos::Lexer<'_, TokenValue>> for Span {
    fn from(value: &logos::Lexer<'_, TokenValue>) -> Self {
        Self {
            range: value.span(),
            line_start: value.extras.line,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token<'inp> {
    pub(crate) value: TokenValue,
    pub(crate) lexeme: &'inp str,
    pub(crate) span: Span,
}
impl<'inp> Token<'inp> {
    pub fn value(&self) -> &TokenValue {
        &self.value
    }

    pub fn span(&self) -> &Span {
        &self.span
    }

    pub fn into_value(self) -> TokenValue {
        self.value
    }

    pub fn lexeme(&self) -> &'inp str {
        self.lexeme
    }
}
impl From<LexerError> for Token<'static> {
    fn from(value: LexerError) -> Self {
        let span = value.span.clone().unwrap_or(Span {
            range: 0..0,
            line_start: 0,
        });
        Self {
            value: TokenValue::Error(value),
            lexeme: "",
            span,
        }
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
        let span = Span {
            range: self.inner.span(),
            line_start: self.inner.extras.line,
        };
        match self.inner.next() {
            Some(Err(LexerError {
                kind: LexerErrorKind::Other,
                ..
            })) => Some(Err(LexerError::from_lexer(&mut self.inner))),
            Some(value) => Some(value.map(|value| Token {
                value,
                lexeme: self.inner.slice(),
                span,
            })),
            None if !self.emitted_eof => {
                self.emitted_eof = true;
                Some(Ok(Token {
                    value: TokenValue::Eof,
                    lexeme: "",
                    span,
                }))
            }
            None => None,
        }
    }
}

#[cfg(test)]
mod test;
