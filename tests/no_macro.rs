use lexit::{
    language::{Language, PairDefinition, PairDirection, TokenBehavior, TokenDefinition},
    lex::{Lexer, Token},
};

#[test]
fn simple_arithmetic_language_manual_definition_test() {
    let definitions = vec![
        TokenDefinition::new(
            "WHITESPACE".to_string(),
            r"\s+",
            TokenBehavior::Ignore,
            0,
            false,
        )
        .unwrap(),
        TokenDefinition::new("PLUS".to_string(), r"\+", TokenBehavior::None, 50, false).unwrap(),
        TokenDefinition::new("MINUS".to_string(), r"-", TokenBehavior::None, 50, false).unwrap(),
        TokenDefinition::new(
            "MULTIPLY".to_string(),
            r"\*",
            TokenBehavior::None,
            50,
            false,
        )
        .unwrap(),
        TokenDefinition::new("DIVIDE".to_string(), r"/", TokenBehavior::None, 50, false).unwrap(),
        TokenDefinition::new(
            "LEFT_PAREN".to_string(),
            r"\(",
            TokenBehavior::Pair(PairDefinition::new(
                PairDirection::Open,
                "RIGHT_PAREN".to_string(),
            )),
            60,
            false,
        )
        .unwrap(),
        TokenDefinition::new(
            "RIGHT_PAREN".to_string(),
            r"\)",
            TokenBehavior::Pair(PairDefinition::new(
                PairDirection::Close,
                "LEFT_PAREN".to_string(),
            )),
            60,
            false,
        )
        .unwrap(),
        TokenDefinition::new(
            "FLOAT_LITERAL".to_string(),
            r"\d+\.\d+",
            TokenBehavior::None,
            70,
            true,
        )
        .unwrap(),
        TokenDefinition::new(
            "INTEGER_LITERAL".to_string(),
            r"\d+",
            TokenBehavior::None,
            65,
            true,
        )
        .unwrap(),
    ];

    let language = Language::new(definitions);
    let lexer = Lexer::new(language.unwrap()).unwrap();
    let input_string = "(1 + 2.5) * 3";
    let tokens_result = lexer.lex(input_string);

    match tokens_result {
        Ok(actual_tokens) => {
            let expected_tokens = vec![
                Token::new("LEFT_PAREN".to_string(), None, 1, 1),
                Token::new("INTEGER_LITERAL".to_string(), Some("1".to_string()), 1, 2),
                Token::new("PLUS".to_string(), None, 1, 4),
                Token::new("FLOAT_LITERAL".to_string(), Some("2.5".to_string()), 1, 6),
                Token::new("RIGHT_PAREN".to_string(), None, 1, 9),
                Token::new("MULTIPLY".to_string(), None, 1, 11),
                Token::new("INTEGER_LITERAL".to_string(), Some("3".to_string()), 1, 13),
            ];

            assert_eq!(
                actual_tokens.len(),
                expected_tokens.len(),
                "Token count mismatch!"
            );

            for (i, expected) in expected_tokens.iter().enumerate() {
                let actual = &actual_tokens[i];
                assert_eq!(
                    actual, expected,
                    "Token mismatch at index {}. Actual: {:?}, Expected: {:?}",
                    i, actual, expected
                );
            }
            println!("All tokens matched successfully for manual language definition!");
        }
        Err(e) => {
            panic!("Lexing failed: {:?}", e);
        }
    }
}
