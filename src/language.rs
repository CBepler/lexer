use std::collections::HashSet;

pub use token_definition::{
    TokenDefinition,
    token_behavior::{
        TokenBehavior,
        comment_end_condition::CommentEndCondition,
        pair_definition::{PairDefinition, PairDirection},
    },
};

#[cfg(test)]
use super::*;

pub mod token_definition;

#[derive(Debug)]
pub struct Language {
    token_definitions: Vec<TokenDefinition>,
}

impl Language {
    pub fn new(tokens: Vec<TokenDefinition>) -> Result<Self, String> {
        let mut token_names = HashSet::new();
        let mut open_pair_names = HashSet::new();
        let mut close_pair_names = HashSet::new();
        let mut expected_pairs = HashSet::<(String, String)>::new();

        for tok in &tokens {
            match &tok.get_behavior() {
                TokenBehavior::Pair(pair_def) => match pair_def.get_pair_type() {
                    PairDirection::Open => {
                        open_pair_names.insert(tok.get_name().to_string());
                        expected_pairs.insert((
                            tok.get_name().to_string(),
                            pair_def.get_counterpart_name().to_string(),
                        ));
                    }
                    PairDirection::Close => {
                        close_pair_names.insert(tok.get_name().to_string());
                        expected_pairs.insert((
                            pair_def.get_counterpart_name().to_string(),
                            tok.get_name().to_string(),
                        ));
                    }
                },
                _ => (),
            };
            if !token_names.insert(tok.get_name().to_string()) {
                return Err(format!("Duplicate token name found: '{}'", tok.get_name()));
            }
        }
        if let Err(e) = Self::check_pairs(&expected_pairs, &open_pair_names, &close_pair_names) {
            return Err(e);
        }

        Ok(Language {
            token_definitions: tokens,
        })
    }

    pub fn new_from_results(tokens: Vec<Result<TokenDefinition, String>>) -> Result<Self, String> {
        let compiled_tokens: Vec<TokenDefinition> = tokens
            .into_iter()
            .collect::<Result<Vec<TokenDefinition>, String>>()?;
        Language::new(compiled_tokens)
    }

    pub fn get_token_definitions(&self) -> &Vec<TokenDefinition> {
        &self.token_definitions
    }

    fn check_pairs(
        expected_pairs: &HashSet<(String, String)>,
        open_pair_names: &HashSet<String>,
        close_pair_names: &HashSet<String>,
    ) -> Result<(), String> {
        for (open_name, close_name) in expected_pairs {
            if !open_pair_names.contains(open_name) {
                return Err(format!(
                    "Paired token error: Expected opener '{}' but it's not defined as an open pair.",
                    open_name
                ));
            }
            if !close_pair_names.contains(close_name) {
                return Err(format!(
                    "Paired token error: Counterpart '{}' for '{}' is not defined as a close pair.",
                    close_name, open_name
                ));
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn language_new_correct_creates_language() {
        let module_result = keyword!("MODULE", r"module\b");
        let identifier_result = store_token!("IDENTIFIER", r"[a-zA-Z_][a-zA-Z0-9_]*");
        let open_paren_result = open_pair!("LEFT_PAREN", r"\(", "RIGHT_PAREN");
        let close_paren_result = close_pair!("RIGHT_PAREN", r"\)", "LEFT_PAREN");
        let input_result = keyword!("INPUT", r"input\b");
        let wire_result = keyword!("WIRE", r"wire\b");
        let comma_result = keyword!("COMMA", r",");
        let output_result = keyword!("OUTPUT", r"output\b");
        let semicolon_result = keyword!("SEMICOLON", r";");
        let end_module_result = keyword!("END_MODULE", r"endmodule\b");
        let single_line_comment_result = single_line_comment!("SINGLE_LINE_COMMENT", r"//");
        let whitespace_result = ignore_token!("WHITESPACE", r"\s+");

        let language_result = define_language! {
            whitespace_result,
            single_line_comment_result,

            module_result,
            input_result,
            output_result,
            wire_result,
            end_module_result,

            open_paren_result,
            close_paren_result,

            comma_result,
            semicolon_result,

            identifier_result,
        };

        print!("{:?}", language_result);
        assert!(language_result.is_ok())
    }

    #[test]
    fn language_new_missing_closing_pair_creates_err() {
        let module_result = keyword!("MODULE", r"module\b");
        let identifier_result = store_token!("IDENTIFIER", r"[a-zA-Z_][a-zA-Z0-9_]*");
        let open_paren_result = open_pair!("LEFT_PAREN", r"\(", "RIGHT_PAREN");
        let input_result = keyword!("INPUT", r"input\b");
        let wire_result = keyword!("WIRE", r"wire\b");
        let comma_result = keyword!("COMMA", r",");
        let output_result = keyword!("OUTPUT", r"output\b");
        let semicolon_result = keyword!("SEMICOLON", r";");
        let end_module_result = keyword!("END_MODULE", r"endmodule\b");
        let single_line_comment_result = single_line_comment!("SINGLE_LINE_COMMENT", r"//");
        let whitespace_result = ignore_token!("WHITESPACE", r"\s+");

        let language_result = define_language! {
            whitespace_result,
            single_line_comment_result,

            module_result,
            input_result,
            output_result,
            wire_result,
            end_module_result,

            open_paren_result,

            comma_result,
            semicolon_result,

            identifier_result,
        };

        assert!(language_result.is_err())
    }

    #[test]
    fn language_new_duplicate_token_creates_err() {
        let module_result = keyword!("MODULE", r"module\b");
        let identifier_result = store_token!("IDENTIFIER", r"[a-zA-Z_][a-zA-Z0-9_]*");
        let open_paren_result = open_pair!("LEFT_PAREN", r"\(", "RIGHT_PAREN");
        let close_paren_result = close_pair!("RIGHT_PAREN", r"\)", "LEFT_PAREN");
        let input_result = keyword!("INPUT", r"input\b");
        let wire_result = keyword!("WIRE", r"wire\b");
        let comma_result = keyword!("COMMA", r",");
        let output_result = keyword!("OUTPUT", r"output\b");
        let semicolon_result = keyword!("SEMICOLON", r";");
        let end_module_result = keyword!("END_MODULE", r"endmodule\b");
        let single_line_comment_result = single_line_comment!("SINGLE_LINE_COMMENT", r"//");
        let comma_result2 = keyword!("COMMA", r",");
        let whitespace_result = ignore_token!("WHITESPACE", r"\s+");

        let language_result = define_language! {
            whitespace_result,
            single_line_comment_result,

            module_result,
            input_result,
            output_result,
            wire_result,
            end_module_result,

            open_paren_result,
            close_paren_result,

            comma_result,
            semicolon_result,
            comma_result2,

            identifier_result,
        };

        assert!(language_result.is_err())
    }
}
