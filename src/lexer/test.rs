use crate::lexer::Lexer;
use itertools::Itertools;

fn test_tokenize(input: &str, expected_tokens: impl IntoIterator<Item = &'static str>) {
    for (token, expected) in Lexer::new(input).zip_eq(expected_tokens) {
        let value = match token {
            Err(error) => error.cc_format(),
            Ok(token) => format!(
                "{} {} {}",
                token.value().diag_name(),
                token.lexeme(),
                token.value().payload().diag_value()
            ),
        };
        assert_eq!(value, expected);
    }
}

#[test]
fn paren() {
    test_tokenize(
        "(()",
        [
            "LEFT_PAREN ( null",
            "LEFT_PAREN ( null",
            "RIGHT_PAREN ) null",
            "EOF  null",
        ],
    );
}

#[test]
fn braces() {
    test_tokenize(
        "{{}}",
        [
            "LEFT_BRACE { null",
            "LEFT_BRACE { null",
            "RIGHT_BRACE } null",
            "RIGHT_BRACE } null",
            "EOF  null",
        ],
    );
}

#[test]
fn single_char_tokens() {
    test_tokenize(
        "({*.,+*})",
        [
            "LEFT_PAREN ( null",
            "LEFT_BRACE { null",
            "STAR * null",
            "DOT . null",
            "COMMA , null",
            "PLUS + null",
            "STAR * null",
            "RIGHT_BRACE } null",
            "RIGHT_PAREN ) null",
            "EOF  null",
        ],
    );
}

#[test]
fn simple_errors() {
    test_tokenize(
        ",.$(#",
        [
            "COMMA , null",
            "DOT . null",
            "[line 1] Error: Unexpected character: $",
            "LEFT_PAREN ( null",
            "[line 1] Error: Unexpected character: #",
            "EOF  null",
        ],
    );
}

#[test]
fn assign_and_equal() {
    test_tokenize(
        "={===}",
        [
            "EQUAL = null",
            "LEFT_BRACE { null",
            "EQUAL_EQUAL == null",
            "EQUAL = null",
            "RIGHT_BRACE } null",
            "EOF  null",
        ],
    );
}

#[test]
fn negation_inequality() {
    test_tokenize(
        "!!===",
        [
            "BANG ! null",
            "BANG_EQUAL != null",
            "EQUAL_EQUAL == null",
            "EOF  null",
        ],
    );
}

#[test]
fn relational_operators() {
    test_tokenize(
        "<<=>>=",
        [
            "LESS < null",
            "LESS_EQUAL <= null",
            "GREATER > null",
            "GREATER_EQUAL >= null",
            "EOF  null",
        ],
    );
}

#[test]
fn comment() {
    test_tokenize(
        r"{
        // Test
        =
    }",
        [
            "LEFT_BRACE { null",
            "EQUAL = null",
            "RIGHT_BRACE } null",
            "EOF  null",
        ],
    );
}

#[test]
fn literals() {
    test_tokenize(
        r#"123 456.789 "abc def""#,
        [
            "NUMBER 123 123.0",
            "NUMBER 456.789 456.789",
            r#"STRING "abc def" abc def"#,
            "EOF  null",
        ],
    );
}

#[test]
fn unterminated_string() {
    test_tokenize(
        r#""test"#,
        ["[line 1] Error: Unterminated string.", "EOF  null"],
    );
}
