use lexer::*;

#[test]
fn c_like_language() {
    let language_result = define_language! {
        // Ignore whitespace
        // Lower priority for general whitespace
        ignore_token!("WHITESPACE", r"^\s+", 10),

        // Comments: Single-line and Multi-line
        // Comments should generally have low priority to allow keywords/identifiers to match first
        // The `ignore_until` macro expects (name, regex_start, regex_end, priority)
        ignore_until!("SINGLE_LINE_COMMENT", r"//.*", r"\n", 5), // Added explicit newline regex and priority
        ignore_until!("MULTI_LINE_COMMENT", r"/\*", r"\*/", 5), // Replaced ignore_until_token, added priority

        // Keywords for control flow and types
        // Keywords should have high priority to prevent them from being matched as identifiers
        keyword!("INT_KEYWORD", r"^int\b", 100),
        keyword!("IF_KEYWORD", r"^if\b", 100),
        keyword!("ELSE_KEYWORD", r"^else\b", 100),
        keyword!("WHILE_KEYWORD", r"^while\b", 100),
        keyword!("RETURN_KEYWORD", r"^return\b", 100),
        keyword!("VOID_KEYWORD", r"^void\b", 100),

        // Operators
        // Comparison operators (like `==`) should have higher priority than their single-character counterparts (`=`)
        keyword!("EQUALS_COMP", r"^==", 95), // Must be before ASSIGN
        keyword!("ASSIGN", r"^=", 90),
        keyword!("PLUS", r"^\+", 90),
        keyword!("MINUS", r"^-", 90),
        keyword!("MULTIPLY", r"^\*", 90),
        keyword!("DIVIDE", r"^/", 90),


        // Parentheses and Braces for grouping and blocks
        // Pairs are typically high priority and don't store their lexeme
        open_pair!("LEFT_PAREN", r"^\(", "RIGHT_PAREN", 80), // Priority moved to last argument
        close_pair!("RIGHT_PAREN", r"^\)", "LEFT_PAREN", 80), // Priority moved to last argument
        open_pair!("LEFT_BRACE", r"^\{", "RIGHT_BRACE", 80), // Priority moved to last argument
        close_pair!("RIGHT_BRACE", r"^\}", "LEFT_BRACE", 80), // Priority moved to last argument

        // Punctuation
        // Punctuation is usually high priority and doesn't store its lexeme
        keyword!("SEMICOLON", r"^;", 70),
        keyword!("COMMA", r"^,", 70),

        // Identifiers (e.g., variable names, function names)
        // Identifiers should generally have a lower priority than keywords to avoid conflicts
        // They need their actual string stored, so `true` for `to_store`
        token!("IDENTIFIER", r"^[a-zA-Z_][a-zA-Z0-9_]*", 60, true),

        // Integer literals
        // Literals also need their value stored
        token!("INTEGER_LITERAL", r"^\d+", 60, true),
    };

    assert!(
        language_result.is_ok(),
        "Failed to define C-like Language: {:?}",
        language_result.unwrap_err()
    );
    println!("Successfully defined C-like Language!");
}
