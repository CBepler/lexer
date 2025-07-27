use lexer::*;

#[test]
fn arithmetic_expression_language() {
    let language_result = define_language! {
        // Ignore whitespace
        // Whitespace is usually low priority and not stored.
        ignore_token!("WHITESPACE", r"^\s+", 10),

        // Numbers (integers and floating-point)
        // FLOAT_LITERAL must have higher priority than INTEGER_LITERAL
        // to ensure "1.23" is matched as a float, not "1" then ".23".
        // Both need their value stored (`true` for `to_store`).
        token!("FLOAT_LITERAL", r"^\d+\.\d+", 100, true),
        token!("INTEGER_LITERAL", r"^\d+", 90, true),

        // Operators
        // Operators are typically high priority, and their lexeme isn't stored.
        keyword!("PLUS", r"^\+", 80),
        keyword!("MINUS", r"^-", 80),
        keyword!("MULTIPLY", r"^\*", 80),
        keyword!("DIVIDE", r"^/", 80),

        // Parentheses for grouping
        // Pairs are usually high priority and don't store their lexeme.
        open_pair!("LEFT_PAREN", r"^\(", "RIGHT_PAREN", 70),
        close_pair!("RIGHT_PAREN", r"^\)", "LEFT_PAREN", 70),
    };

    assert!(
        language_result.is_ok(),
        "Failed to define Arithmetic Expression Language: {:?}",
        language_result.unwrap_err()
    );
    println!("Successfully defined Arithmetic Expression Language!");
}
