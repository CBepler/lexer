use lexer::*;

#[test]
fn json_like_language() {
    let language_result = define_language! {
        // Ignore whitespace
        // Whitespace is usually low priority and not stored.
        ignore_token!("WHITESPACE", r"^\s+", 10),

        // Keywords
        // Keywords are often high priority to prevent them from being matched as identifiers, and their lexeme isn't stored.
        keyword!("TRUE", r"^true\b", 100),
        keyword!("FALSE", r"^false\b", 100),
        keyword!("NULL", r"^null\b", 100),

        // String literals
        // String literals need their actual content stored, hence `true` for `to_store`. They also need a high priority.
        token!("STRING_LITERAL", r#"^"([^"\\]|\\.)*""#, 90, true),

        // Number literals
        // Number literals also need their actual value stored.
        token!("NUMBER_LITERAL", r"^-?\d+(\.\d+)?([eE][+-]?\d+)?", 90, true),

        // Structural elements (paired)
        // Pair symbols like braces and brackets typically don't need their lexeme stored.
        // They are generally high priority.
        open_pair!("LEFT_BRACE", r"^\{", "RIGHT_BRACE", 80),
        close_pair!("RIGHT_BRACE", r"^\}", "LEFT_BRACE", 80),
        open_pair!("LEFT_BRACKET", r"^\[", "RIGHT_BRACKET", 80),
        close_pair!("RIGHT_BRACKET", r"^\]", "LEFT_BRACKET", 80),

        // Punctuation
        // Punctuation marks also usually don't need their lexeme stored.
        // They should have a decent priority to be matched correctly.
        keyword!("COLON", r"^:", 70),
        keyword!("COMMA", r"^,", 70),
    };

    assert!(
        language_result.is_ok(),
        "Failed to define JSON-like Language: {:?}",
        language_result.unwrap_err()
    );
    println!("Successfully defined JSON-like Language!");
}
