use lexer::*;

#[test]
fn verilog_subset_macro() {
    let language_result = define_language! {
        ignore_token!("WHITESPACE", r"^\s+"),
        single_line_comment!("SINGLE_LINE_COMMENT", r"//"),

        keyword!("MODULE", r"^module\b"),
        keyword!("INPUT", r"^input\b"),
        keyword!("OUTPUT", r"^output\b"),
        keyword!("WIRE", r"^wire\b"),
        keyword!("END_MODULE", r"^endmodule\b"),

        open_pair!("LEFT_PAREN", r"^\(", "RIGHT_PAREN"),
        close_pair!("RIGHT_PAREN", r"^\)", "LEFT_PAREN"),

        store_token!("IDENTIFIER", r"^[a-zA-Z_][a-zA-Z0-9_]*"),

        keyword!("COMMA", r"^,"),
        keyword!("SEMICOLON", r"^;"),
    };

    assert!(
        language_result.is_ok(),
        "Failed to define verilog-like Language: {:?}",
        language_result.unwrap_err()
    );
    println!("Successfully defined verilog-like Language!");
}
