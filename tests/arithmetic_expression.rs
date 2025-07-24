use lexer::*;

#[test]
fn arithmetic_expression_language() {
    let language_result = define_language! {
        // Ignore whitespace
        ignore_token!("WHITESPACE", r"^\s+"),

        // Numbers (integers and floating-point)
        store_token!("FLOAT_LITERAL", r"^\d+\.\d+"), // Must come before INTEGER_LITERAL
        store_token!("INTEGER_LITERAL", r"^\d+"),

        // Operators
        keyword!("PLUS", r"^\+"),
        keyword!("MINUS", r"^-"),
        keyword!("MULTIPLY", r"^\*"),
        keyword!("DIVIDE", r"^/"),

        // Parentheses for grouping
        open_pair!("LEFT_PAREN", r"^\(", "RIGHT_PAREN"),
        close_pair!("RIGHT_PAREN", r"^\)", "LEFT_PAREN"),
    };

    assert!(
        language_result.is_ok(),
        "Failed to define Arithmetic Expression Language: {:?}",
        language_result.unwrap_err()
    );
    println!("Successfully defined Arithmetic Expression Language!");
}
