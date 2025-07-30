//! This module provides the Language struct used to represent a lexable language

use std::collections::HashSet;

pub use token_definition::{
    TokenDefinition,
    token_behavior::{
        TokenBehavior,
        pair_definition::{PairDefinition, PairDirection},
    },
};

pub mod token_definition;

/// Represents a defined programming language, containing a collection of token definitions
/// and enforcing rules for valid language construction, especially concerning paired tokens.
#[derive(Debug)]
pub struct Language {
    /// A vector of `TokenDefinition` instances that make up the language.
    pub token_definitions: Vec<TokenDefinition>,
}

impl Language {
    /// Creates a new `Language` instance from a vector of `TokenDefinition`s.
    ///
    /// This constructor performs validation to ensure:
    /// 1. There are no duplicate token names.
    /// 2. All paired tokens (e.g., parentheses, braces) have their counterparts correctly defined
    ///    as either an `Open` or `Close` pair.
    ///
    /// # Arguments
    /// * `tokens`: A `Vec<TokenDefinition>` representing the tokens of the language.
    ///
    /// # Returns
    /// A `Result` which is `Ok(Language)` if all validations pass, or `Err(String)`
    /// with an error message if there's a duplicate token name or a paired token mismatch.
    ///
    /// # Examples
    /// ```
    /// use lexit::language::{Language, TokenDefinition, TokenBehavior, PairDefinition, PairDirection};
    ///
    /// // Example of valid tokens
    /// let ok_tokens = vec![
    ///     TokenDefinition::new("KEYWORD".to_string(), "keyword", TokenBehavior::None, 10, false).unwrap(),
    ///     TokenDefinition::new(
    ///         "OPEN_PAREN".to_string(),
    ///         r"\(",
    ///         TokenBehavior::Pair(PairDefinition::new(PairDirection::Open, "CLOSE_PAREN".to_string())),
    ///         20,
    ///         false,
    ///     ).unwrap(),
    ///     TokenDefinition::new(
    ///         "CLOSE_PAREN".to_string(),
    ///         r"\)",
    ///         TokenBehavior::Pair(PairDefinition::new(PairDirection::Close, "OPEN_PAREN".to_string())),
    ///         20,
    ///         false,
    ///     ).unwrap(),
    /// ];
    /// let language_result = Language::new(ok_tokens);
    /// assert!(language_result.is_ok());
    ///
    /// // Example of duplicate token name
    /// let duplicate_tokens = vec![
    ///     TokenDefinition::new("DUPLICATE".to_string(), "dup", TokenBehavior::None, 10, false).unwrap(),
    ///     TokenDefinition::new("DUPLICATE".to_string(), "dup2", TokenBehavior::None, 10, false).unwrap(),
    /// ];
    /// let error_result = Language::new(duplicate_tokens);
    /// assert!(error_result.is_err());
    /// assert!(error_result.unwrap_err().contains("Duplicate token name"));
    ///
    /// // Example of missing counterpart
    /// let missing_counterpart = vec![
    ///     TokenDefinition::new(
    ///         "OPEN_BRACE".to_string(),
    ///         r"\{",
    ///         TokenBehavior::Pair(PairDefinition::new(PairDirection::Open, "MISSING_BRACE".to_string())),
    ///         20,
    ///         false,
    ///     ).unwrap(),
    /// ];
    /// let error_result_pair = Language::new(missing_counterpart);
    /// assert!(error_result_pair.is_err());
    /// assert!(error_result_pair.unwrap_err().contains("not defined as a close pair"));
    /// ```
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

    /// Creates a new `Language` instance from a vector of `Result<TokenDefinition, String>`.
    ///
    /// This is a convenience constructor that first collects all successful `TokenDefinition`s
    /// and propagates any `Err` results from the token creation process.
    /// After collecting, it then calls `Language::new` to perform the final language validation.
    ///
    /// # Arguments
    /// * `tokens`: A `Vec<Result<TokenDefinition, String>>` where each element is the result
    ///             of attempting to create a `TokenDefinition`.
    ///
    /// # Returns
    /// A `Result` which is `Ok(Language)` if all token definitions are successfully compiled
    /// and the language validation passes. Otherwise, it returns `Err(String)` if any
    /// token definition failed to compile or if the overall language structure is invalid.
    ///
    /// # Examples
    /// ```
    /// use lexit::language::{Language, TokenDefinition, TokenBehavior};
    ///
    /// let ok_results = vec![
    ///     TokenDefinition::new("ID".to_string(), r"[a-z]+", TokenBehavior::None, 10, true),
    ///     TokenDefinition::new("NUM".to_string(), r"[0-9]+", TokenBehavior::None, 10, true),
    /// ];
    /// assert!(Language::new_from_results(ok_results).is_ok());
    ///
    /// let error_results = vec![
    ///     TokenDefinition::new("ID".to_string(), r"[a-z]+", TokenBehavior::None, 10, true),
    ///     TokenDefinition::new("BAD_REGEX".to_string(), r"[", TokenBehavior::None, 10, false),
    /// ];
    /// assert!(Language::new_from_results(error_results).is_err());
    /// ```
    pub fn new_from_results(tokens: Vec<Result<TokenDefinition, String>>) -> Result<Self, String> {
        let compiled_tokens: Vec<TokenDefinition> = tokens
            .into_iter()
            .collect::<Result<Vec<TokenDefinition>, String>>()?;
        Language::new(compiled_tokens)
    }

    /// Returns a reference to the vector of `TokenDefinition`s in this language.
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
    use crate::*;

    #[test]
    fn language_new_correct_creates_language() {
        let module_result = keyword!("MODULE", r"module\b", 100);
        let identifier_result = token!("IDENTIFIER", r"[a-zA-Z_][a-zA-Z0-9_]*", 50, true);
        let open_paren_result = open_pair!("LEFT_PAREN", r"\(", "RIGHT_PAREN", 90);
        let close_paren_result = close_pair!("RIGHT_PAREN", r"\)", "LEFT_PAREN", 90);
        let input_result = keyword!("INPUT", r"input\b", 100);
        let wire_result = keyword!("WIRE", r"wire\b", 100);
        let comma_result = keyword!("COMMA", r",", 70);
        let output_result = keyword!("OUTPUT", r"output\b", 100);
        let semicolon_result = keyword!("SEMICOLON", r";", 70);
        let end_module_result = keyword!("END_MODULE", r"endmodule\b", 100);
        let single_line_comment_result = ignore_until!("SINGLE_LINE_COMMENT", r"//", r"\n", 5);
        let whitespace_result = ignore_token!("WHITESPACE", r"\s+", 10);

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
        let module_result = keyword!("MODULE", r"module\b", 100);
        let identifier_result = token!("IDENTIFIER", r"[a-zA-Z_][a-zA-Z0-9_]*", 50, true);
        let open_paren_result = open_pair!("LEFT_PAREN", r"\(", "RIGHT_PAREN", 90);
        let input_result = keyword!("INPUT", r"input\b", 100);
        let wire_result = keyword!("WIRE", r"wire\b", 100);
        let comma_result = keyword!("COMMA", r",", 70);
        let output_result = keyword!("OUTPUT", r"output\b", 100);
        let semicolon_result = keyword!("SEMICOLON", r";", 70);
        let end_module_result = keyword!("END_MODULE", r"endmodule\b", 100);
        let single_line_comment_result = ignore_until!("SINGLE_LINE_COMMENT", r"//", r"\n", 5);
        let whitespace_result = ignore_token!("WHITESPACE", r"\s+", 10);

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
        let module_result = keyword!("MODULE", r"module\b", 100);
        let identifier_result = token!("IDENTIFIER", r"[a-zA-Z_][a-zA-Z0-9_]*", 50, true);
        let open_paren_result = open_pair!("LEFT_PAREN", r"\(", "RIGHT_PAREN", 90);
        let close_paren_result = close_pair!("RIGHT_PAREN", r"\)", "LEFT_PAREN", 90);
        let input_result = keyword!("INPUT", r"input\b", 100);
        let wire_result = keyword!("WIRE", r"wire\b", 100);
        let comma_result = keyword!("COMMA", r",", 70);
        let output_result = keyword!("OUTPUT", r"output\b", 100);
        let semicolon_result = keyword!("SEMICOLON", r";", 70);
        let end_module_result = keyword!("END_MODULE", r"endmodule\b", 100);
        let single_line_comment_result = ignore_until!("SINGLE_LINE_COMMENT", r"//", r"\n", 5);
        let comma_result2 = keyword!("COMMA", r",", 70);
        let whitespace_result = ignore_token!("WHITESPACE", r"\s+", 10);

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
