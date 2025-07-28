use std::fs;

use lexit::{lex::Lexer, *};

#[test]
fn weird_unicode_language() {
    let language_result = define_language! {
        ignore_token!("WHITESPACE", r"\s+", 10),

        keyword!("TRUTH", r"真", 100),
        keyword!("FALSEHOOD", r"偽", 100),
        keyword!("NOTHING", r"空", 100),

        token!("STRING_LITERAL", r#"«([^»\\]|\\.)*»"#, 90, true),

        token!("NUMBER_LITERAL", r"-?\d+(\.\d+)?([eE][+-]?\d+)?", 90, true),

        open_pair!("LEFT_FLOWER", r"✿", "RIGHT_FLOWER", 80),
        close_pair!("RIGHT_FLOWER", r"❀", "LEFT_FLOWER", 80),
        open_pair!("LEFT_SQUARE", r"⟦", "RIGHT_SQUARE", 80),
        close_pair!("RIGHT_SQUARE", r"⟧", "LEFT_SQUARE", 80),

        keyword!("ARROW", r"⟶", 70),
        keyword!("DELIMITER", r"✧", 70),

        token!("UNICODE_WORD", r"\b世界\b", 90, true),
    };

    assert!(
        language_result.is_ok(),
        "Failed to define Unicode Language: {:?}",
        language_result.unwrap_err()
    );
    println!("Successfully defined Unicode Language!");

    let lexer = Lexer::new(language_result.unwrap()).unwrap();
    let contents = fs::read_to_string("tests/resources/unicode_language.txt").unwrap();
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
        // Line 1: ✿
        ("LEFT_FLOWER", None, 1, 1),
        // Line 2:     «名字» ⟶ «奇異的語言»✧
        ("STRING_LITERAL", Some("«名字»".to_string()), 2, 5),
        ("ARROW", None, 2, 10),
        ("STRING_LITERAL", Some("«奇異的語言»".to_string()), 2, 12),
        ("DELIMITER", None, 2, 19),
        // Line 3:     «版本» ⟶ 1.0✧
        ("STRING_LITERAL", Some("«版本»".to_string()), 3, 5),
        ("ARROW", None, 3, 10),
        ("NUMBER_LITERAL", Some("1.0".to_string()), 3, 12),
        ("DELIMITER", None, 3, 15),
        // Line 4:     «活躍» ⟶ 真✧
        ("STRING_LITERAL", Some("«活躍»".to_string()), 4, 5),
        ("ARROW", None, 4, 10),
        ("TRUTH", None, 4, 12),
        ("DELIMITER", None, 4, 13),
        // Line 5:     «數據» ⟶ ⟦
        ("STRING_LITERAL", Some("«數據»".to_string()), 5, 5),
        ("ARROW", None, 5, 10),
        ("LEFT_SQUARE", None, 5, 12),
        // Line 6:         123✧
        ("NUMBER_LITERAL", Some("123".to_string()), 6, 9),
        ("DELIMITER", None, 6, 12),
        // Line 7:         «項目»✧
        ("STRING_LITERAL", Some("«項目»".to_string()), 7, 9),
        ("DELIMITER", None, 7, 13),
        // Line 8:         偽✧
        ("FALSEHOOD", None, 8, 9),
        ("DELIMITER", None, 8, 10),
        // Line 9:         空
        ("NOTHING", None, 9, 9),
        // Line 10:     ⟧✧
        ("RIGHT_SQUARE", None, 10, 5),
        ("DELIMITER", None, 10, 6),
        // Line 11:     «消息» ⟶ «你好, 世界!»✧
        ("STRING_LITERAL", Some("«消息»".to_string()), 11, 5),
        ("ARROW", None, 11, 10),
        (
            "STRING_LITERAL",
            Some(r#"«你好, 世界!»"#.to_string()),
            11,
            12,
        ),
        ("DELIMITER", None, 11, 21),
        // Line 12:     «探索» ⟶ 世界
        ("STRING_LITERAL", Some("«探索»".to_string()), 12, 5),
        ("ARROW", None, 12, 10),
        ("UNICODE_WORD", Some("世界".to_string()), 12, 12),
        // Line 13: ❀
        ("RIGHT_FLOWER", None, 13, 1),
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
