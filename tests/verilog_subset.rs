use lexer::*;

#[test]
fn verilog_subset_macro() {
    let language_result = define_language! {
        ignore_token!("WHITESPACE", r"^\s+", 0),
        ignore_until!("SINGLE_LINE_COMMENT", r"//", r"\n", 1),

        keyword!("MODULE", r"^module\b", 2),
        keyword!("INPUT", r"^input\b", 2),
        keyword!("OUTPUT", r"^output\b", 2),
        keyword!("WIRE", r"^wire\b", 2),
        keyword!("END_MODULE", r"^endmodule\b", 2),

        open_pair!("LEFT_PAREN", r"^\(", "RIGHT_PAREN", 2),
        close_pair!("RIGHT_PAREN", r"^\)", "LEFT_PAREN", 2),

        token!("IDENTIFIER", r"^[a-zA-Z_][a-zA-Z0-9_]*", 1, true),

        keyword!("COMMA", r"^,", 2),
        keyword!("SEMICOLON", r"^;", 2),
    };

    assert!(
        language_result.is_ok(),
        "Failed to define verilog-like Language: {:?}",
        language_result.unwrap_err()
    );
    println!("Successfully defined verilog-like Language!");
}
