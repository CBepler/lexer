use lexer::*;

#[test]
fn c_like_language() {
    let language_result = define_language! {
        // Ignore whitespace
        ignore_token!("WHITESPACE", r"^\s+"),

        // Comments: Single-line and Multi-line
        single_line_comment!("SINGLE_LINE_COMMENT", r"//.*"),
        // Multi-line comment: Starts with /* and ends with */
        multi_line_comment!("MULTI_LINE_COMMENT", r"/\*", r"\*/"),

        // Keywords for control flow and types
        keyword!("INT_KEYWORD", r"^int\b"),
        keyword!("IF_KEYWORD", r"^if\b"),
        keyword!("ELSE_KEYWORD", r"^else\b"),
        keyword!("WHILE_KEYWORD", r"^while\b"),
        keyword!("RETURN_KEYWORD", r"^return\b"),
        keyword!("VOID_KEYWORD", r"^void\b"),

        // Operators
        keyword!("ASSIGN", r"^="),
        keyword!("PLUS", r"^\+"),
        keyword!("MINUS", r"^-"),
        keyword!("MULTIPLY", r"^\*"),
        keyword!("DIVIDE", r"^/"),
        keyword!("EQUALS_COMP", r"^=="), // Comparison operator (must be before single '=')

        // Parentheses and Braces for grouping and blocks
        open_pair!("LEFT_PAREN", r"^\(", "RIGHT_PAREN"),
        close_pair!("RIGHT_PAREN", r"^\)", "LEFT_PAREN"),
        open_pair!("LEFT_BRACE", r"^\{", "RIGHT_BRACE"),
        close_pair!("RIGHT_BRACE", r"^\}", "LEFT_BRACE"),

        // Punctuation
        keyword!("SEMICOLON", r"^;"),
        keyword!("COMMA", r"^,"),

        // Identifiers (e.g., variable names, function names)
        store_token!("IDENTIFIER", r"^[a-zA-Z_][a-zA-Z0-9_]*"),

        // Integer literals
        store_token!("INTEGER_LITERAL", r"^\d+"),
    };

    assert!(
        language_result.is_ok(),
        "Failed to define C-like Language: {:?}",
        language_result.unwrap_err()
    );
    println!("Successfully defined C-like Language!");
}
