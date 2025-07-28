use crate::regex::Regex;

use token_behavior::TokenBehavior;

pub mod token_behavior;

/// Defines a single token type for a programming language, including its name,
/// regular expression pattern, behavior, priority, and whether its matched text should be stored.
#[derive(Debug, PartialEq)]
pub struct TokenDefinition {
    /// The unique name of the token (e.g., "IDENTIFIER", "KEYWORD").
    pub name: String,
    /// The compiled regular expression used to match this token.
    pub regex: Regex,
    /// The behavior associated with this token (e.g., `Ignore`, `Pair`, `None`).
    pub behavior: TokenBehavior,
    /// The priority of the token, used to resolve ambiguities when multiple
    /// regexes could match the same input. Higher values mean higher priority.
    pub priority: i32,
    /// A flag indicating whether the actual matched string for this token should be stored.
    pub to_store_match: bool,
}

impl TokenDefinition {
    /// Creates a new `TokenDefinition` instance.
    ///
    /// This function compiles the provided regex string into a `Regex` object.
    /// If the `TokenBehavior` is `IgnoreUntil`, it also compiles the `regex_str_condition`.
    ///
    /// # Arguments
    /// * `name`: The name of the token.
    /// * `regex_str`: The regular expression string that defines this token's pattern.
    /// * `behavior`: The `TokenBehavior` for this token.
    /// * `priority`: The priority level of this token.
    /// * `to_store_match`: Whether to store the matched string.
    ///
    /// # Returns
    /// A `Result` which is `Ok(TokenDefinition)` on successful creation, or `Err(String)`
    /// if the provided `regex_str` or `regex_str_condition` (for `IgnoreUntil` behavior)
    /// is an invalid regular expression.
    ///
    /// # Examples
    /// ```
    /// use lexit::language::{TokenDefinition, TokenBehavior, PairDefinition, PairDirection};
    ///
    /// // Simple token
    /// let keyword_token = TokenDefinition::new(
    ///     "IF".to_string(),
    ///     r"if\b",
    ///     TokenBehavior::None,
    ///     100,
    ///     true,
    /// ).unwrap();
    /// assert_eq!(keyword_token.get_name(), "IF");
    ///
    /// // Paired token
    /// let open_brace = TokenDefinition::new(
    ///     "OPEN_BRACE".to_string(),
    ///     r"\{",
    ///     TokenBehavior::Pair(PairDefinition::new(PairDirection::Open, "CLOSE_BRACE".to_string())),
    ///     90,
    ///     false,
    /// ).unwrap();
    ///
    /// // IgnoreUntil token (e.g., for comments)
    /// let comment_token = TokenDefinition::new(
    ///     "SINGLE_LINE_COMMENT".to_string(),
    ///     r"//",
    ///     TokenBehavior::IgnoreUntil(r"\n".to_string()),
    ///     5,
    ///     false,
    /// ).unwrap();
    ///
    /// // Invalid regex
    /// let invalid_token = TokenDefinition::new(
    ///     "BAD_TOKEN".to_string(),
    ///     r"[", // Unclosed bracket is invalid regex
    ///     TokenBehavior::None,
    ///     10,
    ///     false,
    /// );
    /// assert!(invalid_token.is_err());
    /// ```
    pub fn new(
        name: String,
        regex_str: &str,
        behavior: TokenBehavior,
        priority: i32,
        to_store_match: bool,
    ) -> Result<Self, String> {
        let compiled_regex = Regex::new(regex_str)
            .map_err(|e| format!("Invalid regex for token '{}': {}", name, e))?;

        let final_behavior = match behavior {
            TokenBehavior::IgnoreUntil(regex_str_condition) => {
                let compiled_condition = Regex::new(&regex_str_condition).map_err(|e| {
                    format!(
                        "Invalid regex for IgnoreUntil condition for token '{}': {}",
                        name, e
                    )
                })?;
                TokenBehavior::IgnoreUntilCompiled(compiled_condition)
            }
            _ => behavior,
        };
        Ok(TokenDefinition {
            name,
            regex: compiled_regex,
            behavior: final_behavior,
            priority,
            to_store_match,
        })
    }

    /// Returns the name of the token.
    pub fn get_name(&self) -> &str {
        &self.name
    }

    /// Returns a reference to the compiled `Regex` for this token.
    pub fn get_regex(&self) -> &Regex {
        &self.regex
    }

    /// Returns a reference to the `TokenBehavior` of this token.
    pub fn get_behavior(&self) -> &TokenBehavior {
        &self.behavior
    }

    /// Returns the priority of the token.
    pub fn get_priority(&self) -> i32 {
        self.priority
    }

    /// Returns `true` if the matched string for this token should be stored, `false` otherwise.
    pub fn get_to_store_match(&self) -> bool {
        self.to_store_match
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::regex::Regex;

    #[test]
    fn token_definition_new_correct_assert_eq() {
        let d1 = TokenDefinition::new("MOD".to_string(), r"mod", TokenBehavior::None, 100, false);
        let d2 = TokenDefinition::new("MOD".to_string(), r"mod", TokenBehavior::None, 100, false);
        assert_eq!(d1, d2);
    }

    #[test]
    fn token_definition_new_correct_ignore_until_assert_eq() {
        let d1 = TokenDefinition::new(
            "COMMENT".to_string(),
            r"//",
            TokenBehavior::IgnoreUntil(r"\n".to_string()),
            5,
            false,
        );
        let d2 = TokenDefinition::new(
            "COMMENT".to_string(),
            r"//",
            TokenBehavior::IgnoreUntilCompiled(Regex::new(r"\n").unwrap()),
            5,
            false,
        );
        assert_eq!(d1.unwrap(), d2.unwrap());
    }

    #[test]
    fn token_definition_new_invalid_regex_assert_err() {
        let d1 = TokenDefinition::new(
            "BAD_TOKEN".to_string(),
            r"[",
            TokenBehavior::None,
            100,
            false,
        );
        assert!(d1.is_err());
    }

    #[test]
    fn token_definition_new_invalid_ignore_until_regex_assert_err() {
        let d1 = TokenDefinition::new(
            "BAD_COMMENT".to_string(),
            r"/*",
            TokenBehavior::IgnoreUntil(r"[".to_string()),
            5,
            false,
        );
        assert!(d1.is_err());
    }
}
