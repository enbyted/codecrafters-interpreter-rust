use std::{error::Error, iter::Peekable};

use thiserror::Error;

use crate::lexer::{LexerError, Span, Token, TokenKind, TokenValue};

#[derive(Debug, Clone, PartialEq, Error)]
#[error("Expect '{}' {message}.", token.symbol())]
pub(super) struct ExpectedToken {
    pub(super) token: TokenKind,
    pub(super) message: &'static str,
}

#[derive(Debug, Error)]
#[error("Invalid assignment target.")]
pub(super) struct InvalidAssignmentTarget;

#[derive(Debug, Error)]
#[error("[line {line}] Error {}: {inner}", self.location())]
pub(super) struct ParseError {
    lexeme: Option<String>,
    line: usize,
    #[source]
    inner: Box<dyn Error>,
}
impl ParseError {
    fn location(&self) -> String {
        if let Some(lexeme) = &self.lexeme {
            format!("at '{lexeme}'")
        } else {
            format!("at end")
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub(super) struct SpanBegin {
    start: usize,
    line: usize,
}

pub(super) trait Parser {
    fn peek(&mut self) -> &Token<'_>;
    fn advance(&mut self);
    fn current(&self) -> &Token<'_>;
    fn previous(&self) -> &Token<'_>;

    fn is_at_end(&self) -> bool;

    fn error_here(&self, error: impl Error + 'static) -> ParseError {
        let cur = self.current();
        ParseError {
            lexeme: (!self.is_at_end()).then(|| cur.lexeme().to_string()),
            line: cur.span().start_line(),
            inner: Box::new(error),
        }
    }

    fn begin_span(&self) -> SpanBegin {
        let cur = self.current();
        SpanBegin {
            start: cur.span().start(),
            line: cur.span().start_line(),
        }
    }

    fn end_span(&self, span: SpanBegin) -> Span {
        let cur = self.current();
        Span {
            range: span.start..cur.span().end(),
            line_start: span.line,
        }
    }

    fn take(&mut self, kind: TokenKind) -> Option<TokenValue> {
        if self.current().value() == kind {
            self.advance();
            Some(self.previous().value().clone())
        } else {
            None
        }
    }

    fn consume(&mut self, kind: TokenKind, message: &'static str) -> Result<(), ParseError> {
        if self.current().value() == kind {
            self.advance();
            Ok(())
        } else {
            Err(self.error_here(ExpectedToken {
                token: kind,
                message,
            }))
        }
    }

    fn take_if<T>(&mut self, pred: impl FnOnce(&TokenValue) -> Option<T>) -> Option<T> {
        if let Some(value) = pred(self.current().value()) {
            self.advance();
            Some(value)
        } else {
            None
        }
    }
}

pub(super) struct ParserState<'inp, L: Iterator> {
    lexer: L,
    peeked: Option<Token<'inp>>,
    current: Token<'inp>,
    previous: Option<Token<'inp>>,
}
impl<'inp, L> ParserState<'inp, L>
where
    L: Iterator<Item = Result<Token<'inp>, LexerError>>,
{
    pub(super) fn new(mut lexer: L) -> Self {
        let mut this = Self {
            current: Self::map_token(lexer.next().expect("Lexer must emit at least one token")),
            lexer,
            peeked: None,
            previous: None,
        };
        this.peek();
        this
    }

    fn take_peeked_if_not_eof(&mut self) -> Option<Token<'inp>> {
        let value = self.peeked.take();
        if matches!(
            value,
            Some(Token {
                value: TokenValue::Eof,
                ..
            })
        ) {
            self.peeked = value.clone();
        }

        value
    }

    fn map_token(token: Result<Token<'inp>, LexerError>) -> Token<'inp> {
        match token {
            Ok(token) => token,
            Err(err) => err.into(),
        }
    }
}

impl<'inp, L> Parser for ParserState<'inp, L>
where
    L: Iterator<Item = Result<Token<'inp>, LexerError>>,
{
    fn peek(&mut self) -> &Token<'inp> {
        if self.peeked.is_none() {
            if matches!(self.current.value(), TokenValue::Eof) {
                self.peeked = Some(self.current.clone());
            } else {
                self.peeked = self.lexer.next().map(Self::map_token);
            }
        }

        self.peeked
            .as_ref()
            .expect("The lexer should return at least the EOF token")
    }

    fn advance(&mut self) {
        self.peek();
        let next = self
            .take_peeked_if_not_eof()
            .expect("Should always have next token");
        let previous = std::mem::replace(&mut self.current, next);
        self.previous = Some(previous);
    }

    fn current(&self) -> &Token<'inp> {
        &self.current
    }

    fn previous(&self) -> &Token<'inp> {
        self.previous
            .as_ref()
            .expect("previous() may only be called after at least one call to next()")
    }

    fn is_at_end(&self) -> bool {
        matches!(self.current.value(), TokenValue::Eof)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn extends_eof() {
        let mut parser = ParserState::new(
            [Ok(Token {
                value: TokenValue::Eof,
                lexeme: "",
                span: Span::empty(),
            })]
            .into_iter(),
        );

        assert_eq!(parser.current().value(), &TokenValue::Eof);
        assert_eq!(parser.peek().value(), &TokenValue::Eof);
        parser.advance();
        assert_eq!(parser.current().value(), &TokenValue::Eof);
        parser.advance();
        assert_eq!(parser.current().value(), &TokenValue::Eof);
    }

    #[test]
    fn basic_operation() {
        let mut parser = ParserState::new(
            [
                Ok(Token {
                    value: TokenValue::KwVar,
                    lexeme: "",
                    span: Span::empty(),
                }),
                Ok(Token {
                    value: TokenValue::KwClass,
                    lexeme: "",
                    span: Span::empty(),
                }),
                Ok(Token {
                    value: TokenValue::KwElse,
                    lexeme: "",
                    span: Span::empty(),
                }),
                Ok(Token {
                    value: TokenValue::Eof,
                    lexeme: "",
                    span: Span::empty(),
                }),
            ]
            .into_iter(),
        );

        assert!(!parser.is_at_end());
        assert_eq!(parser.current().value(), &TokenValue::KwVar);
        assert_eq!(parser.peek().value(), &TokenValue::KwClass);

        parser.advance();
        assert!(!parser.is_at_end());
        assert_eq!(parser.current().value(), &TokenValue::KwClass);
        assert_eq!(parser.previous().value(), &TokenValue::KwVar);

        parser.advance();
        assert_eq!(parser.current().value(), &TokenValue::KwElse);
        assert_eq!(parser.previous().value(), &TokenValue::KwClass);
        assert!(!parser.is_at_end());

        assert_eq!(parser.peek().value(), &TokenValue::Eof);
        assert!(!parser.is_at_end());
        assert_eq!(parser.current().value(), &TokenValue::KwElse);
        assert_eq!(parser.previous().value(), &TokenValue::KwClass);

        parser.advance();
        assert!(parser.is_at_end());
        assert_eq!(parser.current().value(), &TokenValue::Eof);
        assert_eq!(parser.previous().value(), &TokenValue::KwElse);
        assert_eq!(parser.peek().value(), &TokenValue::Eof);

        parser.advance();
        assert!(parser.is_at_end());
        assert_eq!(parser.current().value(), &TokenValue::Eof);
        assert_eq!(parser.previous().value(), &TokenValue::Eof);
        assert_eq!(parser.peek().value(), &TokenValue::Eof);
    }
}
