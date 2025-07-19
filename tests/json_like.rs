use lexor::*;

#[test]
fn json_like_language() {
    let language_result = define_language! {
        // Ignore whitespace
        ignore_token!("WHITESPACE", r"^\s+"),

        // Keywords
        keyword!("TRUE", r"^true\b"),
        keyword!("FALSE", r"^false\b"),
        keyword!("NULL", r"^null\b"),

        // String literals (basic, doesn't handle all JSON escapes but covers common ones)
        // Matches " followed by any character that is not " or \
        // or an escaped character (\\, \", \/, \b, \f, \n, \r, \t, \uXXXX)
        // followed by "
        store_token!("STRING_LITERAL", r#"^"([^"\\]|\\.)*""#),

        // Number literals (integers and floats, simplified)
        store_token!("NUMBER_LITERAL", r"^-?\d+(\.\d+)?([eE][+-]?\d+)?"),

        // Structural elements (paired)
        open_pair!("LEFT_BRACE", r"^\{", "RIGHT_BRACE"),
        close_pair!("RIGHT_BRACE", r"^\}", "LEFT_BRACE"),
        open_pair!("LEFT_BRACKET", r"^\[", "RIGHT_BRACKET"),
        close_pair!("RIGHT_BRACKET", r"^\]", "LEFT_BRACKET"),

        // Punctuation
        keyword!("COLON", r"^:"),
        keyword!("COMMA", r"^,"),
    };

    assert!(
        language_result.is_ok(),
        "Failed to define JSON-like Language: {:?}",
        language_result.unwrap_err()
    );
    println!("Successfully defined JSON-like Language!");
}
