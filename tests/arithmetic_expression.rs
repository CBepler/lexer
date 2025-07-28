use lexit::{lex::Lexer, *};

#[test]
fn arithmetic_expression_language() {
    let language_result = define_language! {
        ignore_token!("WHITESPACE", r"\s+", 10),

        token!("FLOAT_LITERAL", r"\d+\.\d+", 100, true),
        token!("INTEGER_LITERAL", r"\d+", 90, true),

        keyword!("PLUS", r"\+", 80),
        keyword!("MINUS", r"-", 80),
        keyword!("MULTIPLY", r"\*", 80),
        keyword!("DIVIDE", r"/", 80),

        open_pair!("LEFT_PAREN", r"\(", "RIGHT_PAREN", 70),
        close_pair!("RIGHT_PAREN", r"\)", "LEFT_PAREN", 70),
    };

    assert!(
        language_result.is_ok(),
        "Failed to define Arithmetic Expression Language: {:?}",
        language_result.unwrap_err()
    );
    println!("Successfully defined Arithmetic Expression Language!");

    let lexer = Lexer::new(language_result.unwrap()).unwrap();

    // --- Test Cases ---

    // Test 1: Basic valid expression
    let input1 = "(10 + 5.5) * 2 / (3 - 1)";
    println!("\nLexing input: \"{}\"", input1);
    let tokens1 = lexer.lex(input1).expect("Lexing failed for input1");
    for tok in &tokens1 {
        println!("{}", tok);
    }

    let expected_tokens1 = vec![
        ("LEFT_PAREN", None, 1, 1),
        ("INTEGER_LITERAL", Some("10".to_string()), 1, 2),
        ("PLUS", None, 1, 5),
        ("FLOAT_LITERAL", Some("5.5".to_string()), 1, 7),
        ("RIGHT_PAREN", None, 1, 10),
        ("MULTIPLY", None, 1, 12),
        ("INTEGER_LITERAL", Some("2".to_string()), 1, 14),
        ("DIVIDE", None, 1, 16),
        ("LEFT_PAREN", None, 1, 18),
        ("INTEGER_LITERAL", Some("3".to_string()), 1, 19),
        ("MINUS", None, 1, 21),
        ("INTEGER_LITERAL", Some("1".to_string()), 1, 23),
        ("RIGHT_PAREN", None, 1, 24),
    ];

    assert_eq!(
        tokens1.len(),
        expected_tokens1.len(),
        "Token count mismatch for input1"
    );
    for (i, expected) in expected_tokens1.iter().enumerate() {
        let actual = &tokens1[i];
        assert_eq!(
            actual.get_name(),
            expected.0,
            "Name mismatch at token {} for input1",
            i
        );
        assert_eq!(
            actual.get_match(),
            &expected.1,
            "Match mismatch at token {} for input1",
            i
        );
        assert_eq!(
            actual.get_row(),
            expected.2,
            "Row mismatch at token {} for input1",
            i
        );
        assert_eq!(
            actual.get_col(),
            expected.3,
            "Col mismatch at token {} for input1",
            i
        );
    }

    // Test 2: Expression with leading/trailing whitespace and multiple lines
    let input2 = "  1 + 2\n* 3.0 ";
    println!("\nLexing input: \"{}\"", input2);
    let tokens2 = lexer.lex(input2).expect("Lexing failed for input2");
    for tok in &tokens2 {
        println!("{}", tok);
    }

    let expected_tokens2 = vec![
        ("INTEGER_LITERAL", Some("1".to_string()), 1, 3),
        ("PLUS", None, 1, 5),
        ("INTEGER_LITERAL", Some("2".to_string()), 1, 7),
        ("MULTIPLY", None, 2, 1),
        ("FLOAT_LITERAL", Some("3.0".to_string()), 2, 3),
    ];

    assert_eq!(
        tokens2.len(),
        expected_tokens2.len(),
        "Token count mismatch for input2"
    );
    for (i, expected) in expected_tokens2.iter().enumerate() {
        let actual = &tokens2[i];
        assert_eq!(
            actual.get_name(),
            expected.0,
            "Name mismatch at token {} for input2",
            i
        );
        assert_eq!(
            actual.get_match(),
            &expected.1,
            "Match mismatch at token {} for input2",
            i
        );
        assert_eq!(
            actual.get_row(),
            expected.2,
            "Row mismatch at token {} for input2",
            i
        );
        assert_eq!(
            actual.get_col(),
            expected.3,
            "Col mismatch at token {} for input2",
            i
        );
    }

    // Test 3: Unclosed parenthesis error
    let input3 = "(1 + 2";
    println!("\nLexing input: \"{}\"", input3);
    let error_result3 = lexer.lex(input3);
    assert!(
        error_result3.is_err(),
        "Expected error for unclosed parenthesis, but got success for input3"
    );
    assert!(
        error_result3
            .unwrap_err()
            .contains("Unclosed open pair: LEFT_PAREN"),
        "Error message mismatch for input3"
    );

    // Test 4: Unexpected character error
    let input4 = "10 $ 5";
    println!("\nLexing input: \"{}\"", input4);
    let error_result4 = lexer.lex(input4);
    assert!(
        error_result4.is_err(),
        "Expected error for unexpected character, but got success for input4"
    );
    assert!(
        error_result4
            .unwrap_err()
            .contains("Unexpected character at row 1 col 4: '$'"),
        "Error message mismatch for input4"
    );

    // Test 5: Float without leading digit (invalid for current regex)
    let input5 = ".5";
    println!("\nLexing input: \"{}\"", input5);
    let error_result5 = lexer.lex(input5);
    assert!(
        error_result5.is_err(),
        "Expected error for .5, but got success for input5"
    );
    assert!(
        error_result5
            .unwrap_err()
            .contains("Unexpected character at row 1 col 1: '.'"),
        "Error message mismatch for input5"
    );

    // Test 6: Empty string
    let input6 = "";
    println!("\nLexing input: \"{}\"", input6);
    let tokens6 = lexer.lex(input6).expect("Lexing failed for empty string");
    assert!(
        tokens6.is_empty(),
        "Expected no tokens for empty string, but got some for input6"
    );
    println!("Tokens: {:#?}", tokens6);
}
