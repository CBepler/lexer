use std::fs;

use lexit::{lex::Lexer, *};

#[test]
fn verilog_subset_macro() {
    let language_result = define_language! {
        ignore_token!("WHITESPACE", r"\s+", 0),
        ignore_until!("SINGLE_LINE_COMMENT", r"//", r"\n", 1),
        ignore_until!("MULTI_LINE_COMMENT", r"/\*", r"\*/", 1),


        keyword!("MODULE", r"\bmodule\b", 100),
        keyword!("INPUT", r"\binput\b", 100),
        keyword!("OUTPUT", r"\boutput\b", 100),
        keyword!("WIRE", r"\bwire\b", 100),
        keyword!("ASSIGN_KW", r"\bassign\b", 100),
        keyword!("END_MODULE", r"\bendmodule\b", 100),

        keyword!("ASSIGN_OP", r"=", 90),
        keyword!("AND_OP", r"&", 90),
        keyword!("OR_OP", r"\|", 90),
        keyword!("XOR_OP", r"\^", 90),
        keyword!("NOT_OP", r"~", 90),

        open_pair!("LEFT_PAREN", r"\(", "RIGHT_PAREN", 80),
        close_pair!("RIGHT_PAREN", r"\)", "LEFT_PAREN", 80),

        keyword!("COMMA", r",", 70),
        keyword!("SEMICOLON", r";", 70),

        token!("IDENTIFIER", r"[a-zA-Z_][a-zA-Z0-9_]*\b", 60, true),

        token!("INTEGER_LITERAL", r"\d+", 60, true),
    };

    assert!(
        language_result.is_ok(),
        "Failed to define Verilog-like Language: {:?}",
        language_result.unwrap_err()
    );
    println!("Successfully defined Verilog-like Language!");

    let lexer = Lexer::new(language_result.unwrap()).unwrap();
    let contents = fs::read_to_string("tests/resources/verilog_simple.v").unwrap();
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
        // Line 1: module simple_logic (
        ("MODULE", None, 1, 1),
        ("IDENTIFIER", Some("simple_logic".to_string()), 1, 8),
        ("LEFT_PAREN", None, 1, 21),
        // Line 2:     input wire a,
        ("INPUT", None, 2, 5),
        ("WIRE", None, 2, 11),
        ("IDENTIFIER", Some("a".to_string()), 2, 16),
        ("COMMA", None, 2, 17),
        // Line 3:     input wire b,
        ("INPUT", None, 3, 5),
        ("WIRE", None, 3, 11),
        ("IDENTIFIER", Some("b".to_string()), 3, 16),
        ("COMMA", None, 3, 17),
        // Line 4:     input wire c,
        ("INPUT", None, 4, 5),
        ("WIRE", None, 4, 11),
        ("IDENTIFIER", Some("c".to_string()), 4, 16),
        ("COMMA", None, 4, 17),
        // Line 5:     input wire sel,
        ("INPUT", None, 5, 5),
        ("WIRE", None, 5, 11),
        ("IDENTIFIER", Some("sel".to_string()), 5, 16),
        ("COMMA", None, 5, 19),
        // Line 6:     output wire out_and_or,
        ("OUTPUT", None, 6, 5),
        ("WIRE", None, 6, 12),
        ("IDENTIFIER", Some("out_and_or".to_string()), 6, 17),
        ("COMMA", None, 6, 27),
        // Line 7:     output wire out_xor_inv
        ("OUTPUT", None, 7, 5),
        ("WIRE", None, 7, 12),
        ("IDENTIFIER", Some("out_xor_inv".to_string()), 7, 17),
        // Line 8: );
        ("RIGHT_PAREN", None, 8, 1),
        ("SEMICOLON", None, 8, 2),
        // Line 10: wire temp_and;
        ("WIRE", None, 10, 5),
        ("IDENTIFIER", Some("temp_and".to_string()), 10, 10),
        ("SEMICOLON", None, 10, 18),
        // Line 11: wire temp_or;
        ("WIRE", None, 11, 5),
        ("IDENTIFIER", Some("temp_or".to_string()), 11, 10),
        ("SEMICOLON", None, 11, 17),
        // Line 12: wire temp_not_c;
        ("WIRE", None, 12, 5),
        ("IDENTIFIER", Some("temp_not_c".to_string()), 12, 10),
        ("SEMICOLON", None, 12, 20),
        // Line 16: assign temp_and = a & b;
        ("ASSIGN_KW", None, 16, 5),
        ("IDENTIFIER", Some("temp_and".to_string()), 16, 12),
        ("ASSIGN_OP", None, 16, 21),
        ("IDENTIFIER", Some("a".to_string()), 16, 23),
        ("AND_OP", None, 16, 25),
        ("IDENTIFIER", Some("b".to_string()), 16, 27),
        ("SEMICOLON", None, 16, 28),
        // Line 17: assign temp_or  = a | b;
        ("ASSIGN_KW", None, 17, 5),
        ("IDENTIFIER", Some("temp_or".to_string()), 17, 12),
        ("ASSIGN_OP", None, 17, 21),
        ("IDENTIFIER", Some("a".to_string()), 17, 23),
        ("OR_OP", None, 17, 25),
        ("IDENTIFIER", Some("b".to_string()), 17, 27),
        ("SEMICOLON", None, 17, 28),
        // Line 21: assign temp_not_c = ~c;
        ("ASSIGN_KW", None, 21, 5),
        ("IDENTIFIER", Some("temp_not_c".to_string()), 21, 12),
        ("ASSIGN_OP", None, 21, 23),
        ("NOT_OP", None, 21, 25),
        ("IDENTIFIER", Some("c".to_string()), 21, 26),
        ("SEMICOLON", None, 21, 27),
        // Line 25: assign out_and_or = temp_and | temp_or;
        ("ASSIGN_KW", None, 25, 5),
        ("IDENTIFIER", Some("out_and_or".to_string()), 25, 12),
        ("ASSIGN_OP", None, 25, 23),
        ("IDENTIFIER", Some("temp_and".to_string()), 25, 25),
        ("OR_OP", None, 25, 34),
        ("IDENTIFIER", Some("temp_or".to_string()), 25, 36),
        ("SEMICOLON", None, 25, 43),
        // Line 29: assign out_xor_inv = b ^ temp_not_c;
        ("ASSIGN_KW", None, 29, 5),
        ("IDENTIFIER", Some("out_xor_inv".to_string()), 29, 12),
        ("ASSIGN_OP", None, 29, 24),
        ("IDENTIFIER", Some("b".to_string()), 29, 26),
        ("XOR_OP", None, 29, 28),
        ("IDENTIFIER", Some("temp_not_c".to_string()), 29, 30),
        ("SEMICOLON", None, 29, 40),
        // Line 31: endmodule
        ("END_MODULE", None, 31, 1),
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
