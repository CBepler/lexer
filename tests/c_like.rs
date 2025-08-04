use std::fs;

use lexit::{lex::Lexer, *};

#[test]
fn c_like_language() {
    let language_result = define_language! {
        ignore_token!("WHITESPACE", r"\s+", 10),

        ignore_until!("SINGLE_LINE_COMMENT", r"//", r"\n", 5),
        ignore_until!("MULTI_LINE_COMMENT", r"/\*", r"\*/", 5),

        keyword!("INT_KEYWORD", r"\bint\b", 100),
        keyword!("IF_KEYWORD", r"\bif\b", 100),
        keyword!("ELSE_KEYWORD", r"\belse\b", 100),
        keyword!("WHILE_KEYWORD", r"\bwhile\b", 100),
        keyword!("RETURN_KEYWORD", r"\breturn\b", 100),
        keyword!("VOID_KEYWORD", r"\bvoid\b", 100),

        keyword!("EQUALS_COMP", r"==", 95),
        keyword!("LESS_THAN_COMP", r"<", 90),
        keyword!("ASSIGN", r"=", 90),
        keyword!("PLUS", r"\+", 90),
        keyword!("MINUS", r"-", 90),
        keyword!("MULTIPLY", r"\*", 90),
        keyword!("DIVIDE", r"/", 90),


        open_pair!("LEFT_PAREN", r"\(", "RIGHT_PAREN", 80),
        close_pair!("RIGHT_PAREN", r"\)", "LEFT_PAREN", 80),
        open_pair!("LEFT_BRACE", r"\{", "RIGHT_BRACE", 80),
        close_pair!("RIGHT_BRACE", r"\}", "LEFT_BRACE", 80),

        keyword!("SEMICOLON", r";", 70),
        keyword!("COMMA", r",", 70),

        token!("IDENTIFIER", r"[a-zA-Z_][a-zA-Z0-9_]*", 60, true),

        token!("INTEGER_LITERAL", r"\d+", 60, true),
    };

    assert!(
        language_result.is_ok(),
        "Failed to define C-like Language: {:?}",
        language_result.unwrap_err()
    );
    println!("Successfully defined C-like Language!");
    let language = language_result.unwrap();
    let lexer = Lexer::new(language).unwrap();
    let contents = fs::read_to_string("tests/resources/c_simple.c").unwrap();
    let tokens = lexer.lex(&contents);
    let token_iter = lexer.lex_iter(&contents);

    assert_eq!(tokens, token_iter.collect());

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
        ("INT_KEYWORD", None, 5, 1),
        ("IDENTIFIER", Some("main".to_string()), 5, 5),
        ("LEFT_PAREN", None, 5, 9),
        ("RIGHT_PAREN", None, 5, 10),
        ("LEFT_BRACE", None, 5, 12),
        ("INT_KEYWORD", None, 6, 5),
        ("IDENTIFIER", Some("x".to_string()), 6, 9),
        ("ASSIGN", None, 6, 11),
        ("INTEGER_LITERAL", Some("10".to_string()), 6, 13),
        ("SEMICOLON", None, 6, 15),
        // Line 7: int y = 20;
        ("INT_KEYWORD", None, 7, 5),
        ("IDENTIFIER", Some("y".to_string()), 7, 9),
        ("ASSIGN", None, 7, 11),
        ("INTEGER_LITERAL", Some("20".to_string()), 7, 13),
        ("SEMICOLON", None, 7, 15),
        // Line 11: if (x == y) {
        ("IF_KEYWORD", None, 11, 5),
        ("LEFT_PAREN", None, 11, 8),
        ("IDENTIFIER", Some("x".to_string()), 11, 9),
        ("EQUALS_COMP", None, 11, 11),
        ("IDENTIFIER", Some("y".to_string()), 11, 14),
        ("RIGHT_PAREN", None, 11, 15),
        ("LEFT_BRACE", None, 11, 17),
        // Line 12: return 0;
        ("RETURN_KEYWORD", None, 12, 9),
        ("INTEGER_LITERAL", Some("0".to_string()), 12, 16),
        ("SEMICOLON", None, 12, 17),
        // Line 13: } else {
        ("RIGHT_BRACE", None, 13, 5),
        ("ELSE_KEYWORD", None, 13, 7),
        ("LEFT_BRACE", None, 13, 12),
        // Line 14: while (x < y) {
        ("WHILE_KEYWORD", None, 14, 9),
        ("LEFT_PAREN", None, 14, 15),
        ("IDENTIFIER", Some("x".to_string()), 14, 16),
        ("LESS_THAN_COMP", None, 14, 18),
        ("IDENTIFIER", Some("y".to_string()), 14, 20),
        ("RIGHT_PAREN", None, 14, 21),
        ("LEFT_BRACE", None, 14, 23),
        // Line 15: x = x + 1;
        ("IDENTIFIER", Some("x".to_string()), 15, 13),
        ("ASSIGN", None, 15, 15),
        ("IDENTIFIER", Some("x".to_string()), 15, 17),
        ("PLUS", None, 15, 19),
        ("INTEGER_LITERAL", Some("1".to_string()), 15, 21),
        ("SEMICOLON", None, 15, 22),
        // Line 16: }
        ("RIGHT_BRACE", None, 16, 9),
        // Line 17: return x;
        ("RETURN_KEYWORD", None, 17, 9),
        ("IDENTIFIER", Some("x".to_string()), 17, 16),
        ("SEMICOLON", None, 17, 17),
        // Line 18: }
        ("RIGHT_BRACE", None, 18, 5),
        // Line 19: } (end of main)
        ("RIGHT_BRACE", None, 19, 1),
    ];

    assert_eq!(
        tokens.len(),
        expected_tokens.len(),
        "Number of tokens mismatched!"
    );

    for (i, expected_token_data) in expected_tokens.into_iter().enumerate() {
        let (expected_name, expected_match, expected_row, expected_col) = expected_token_data;
        let actual_token = &tokens[i];

        assert_eq!(
            actual_token.get_name(),
            expected_name,
            "Token name mismatch at index {}",
            i
        );
        assert_eq!(
            actual_token.get_match(),
            &expected_match,
            "Token match mismatch at index {}",
            i
        );
        assert_eq!(
            actual_token.get_row(),
            expected_row,
            "Token row mismatch at index {}",
            i
        );
        assert_eq!(
            actual_token.get_col(),
            expected_col,
            "Token col mismatch at index {}",
            i
        );
    }
}
