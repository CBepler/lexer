use std::fs;

use lexit::{lex::Lexer, *};

#[test]
fn json_like_language() {
    let language_result = define_language! {
        ignore_token!("WHITESPACE", r"\s+", 10),

        keyword!("TRUE", r"true\b", 100),
        keyword!("FALSE", r"false\b", 100),
        keyword!("NULL", r"null\b", 100),

        token!("STRING_LITERAL", r#""([^"\\]|\\.)*""#, 90, true),

        token!("NUMBER_LITERAL", r"-?\d+(\.\d+)?([eE][+-]?\d+)?", 90, true),

        open_pair!("LEFT_BRACE", r"\{", "RIGHT_BRACE", 80),
        close_pair!("RIGHT_BRACE", r"\}", "LEFT_BRACE", 80),
        open_pair!("LEFT_BRACKET", r"\[", "RIGHT_BRACKET", 80),
        close_pair!("RIGHT_BRACKET", r"\]", "LEFT_BRACKET", 80),

        keyword!("COLON", r":", 70),
        keyword!("COMMA", r",", 70),
    };

    assert!(
        language_result.is_ok(),
        "Failed to define JSON-like Language: {:?}",
        language_result.unwrap_err()
    );
    println!("Successfully defined JSON-like Language!");

    let lexer = Lexer::new(language_result.unwrap()).unwrap();
    let contents = fs::read_to_string("tests/resources/json_simple.json").unwrap();
    let tokens = lexer.lex(&contents);

    if tokens.is_err() {
        eprintln!("Lexing error: {:?}", tokens.unwrap_err());
        panic!("Lexing failed!");
    }

    let tokens = tokens.unwrap();
    println!("Tokens:");
    for tok in &tokens {
        println!("{}", tok);
    }

    let expected_tokens = vec![
        // Line 1: {
        ("LEFT_BRACE", None, 1, 1),
        // Line 2:     "name": "Lexor Demo",
        ("STRING_LITERAL", Some("\"name\"".to_string()), 2, 5),
        ("COLON", None, 2, 11),
        ("STRING_LITERAL", Some("\"Lexor Demo\"".to_string()), 2, 13),
        ("COMMA", None, 2, 25),
        // Line 3:     "version": 1.0,
        ("STRING_LITERAL", Some("\"version\"".to_string()), 3, 5),
        ("COLON", None, 3, 14),
        ("NUMBER_LITERAL", Some("1.0".to_string()), 3, 16),
        ("COMMA", None, 3, 19),
        // Line 4:     "active": true,
        ("STRING_LITERAL", Some("\"active\"".to_string()), 4, 5),
        ("COLON", None, 4, 13),
        ("TRUE", None, 4, 15),
        ("COMMA", None, 4, 19),
        // Line 5:     "data": [
        ("STRING_LITERAL", Some("\"data\"".to_string()), 5, 5),
        ("COLON", None, 5, 11),
        ("LEFT_BRACKET", None, 5, 13),
        // Line 6:         123,
        ("NUMBER_LITERAL", Some("123".to_string()), 6, 9),
        ("COMMA", None, 6, 12),
        // Line 7:         "item",
        ("STRING_LITERAL", Some("\"item\"".to_string()), 7, 9),
        ("COMMA", None, 7, 15),
        // Line 8:         false,
        ("FALSE", None, 8, 9),
        ("COMMA", None, 8, 14),
        // Line 9:         null
        ("NULL", None, 9, 9),
        // Line 10:     ],
        ("RIGHT_BRACKET", None, 10, 5),
        ("COMMA", None, 10, 6),
        // Line 11:     "message": "Hello, \"World\"!"
        ("STRING_LITERAL", Some("\"message\"".to_string()), 11, 5),
        ("COLON", None, 11, 14),
        // Note: The `\"` inside the string literal should be part of the `to_store` value
        (
            "STRING_LITERAL",
            Some(r#""Hello, \"World\"!""#.to_string()),
            11,
            16,
        ),
        // Line 12: }
        ("RIGHT_BRACE", None, 12, 1),
    ];

    assert_eq!(tokens.len(), expected_tokens.len(), "Token count mismatch!");

    for (i, expected_token_data) in expected_tokens.into_iter().enumerate() {
        let (expected_name, expected_match, expected_row, expected_col) = expected_token_data;
        let actual_token = &tokens[i];

        assert_eq!(
            actual_token.get_name(),
            expected_name,
            "Name mismatch at token {} (Expected: {}, Got: {})",
            i,
            expected_name,
            actual_token.get_name()
        );
        assert_eq!(
            actual_token.get_match(),
            &expected_match,
            "Match mismatch at token {} (Expected: {:?}, Got: {:?})",
            i,
            expected_match,
            actual_token.get_match()
        );
        assert_eq!(
            actual_token.get_row(),
            expected_row,
            "Row mismatch at token {} (Expected: {}, Got: {})",
            i,
            expected_row,
            actual_token.get_row()
        );
        assert_eq!(
            actual_token.get_col(),
            expected_col,
            "Col mismatch at token {} (Expected: {}, Got: {})",
            i,
            expected_col,
            actual_token.get_col()
        );
    }
    println!("All tokens matched successfully!");
}
