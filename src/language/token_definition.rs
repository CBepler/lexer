use crate::regex::Regex;

use token_behavior::TokenBehavior;

pub mod token_behavior;

#[derive(Debug, PartialEq)]
pub struct TokenDefinition {
    pub name: String,
    pub regex: Regex,
    pub behavior: TokenBehavior,
    pub priority: i32,
    pub to_store_match: bool,
}

impl TokenDefinition {
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

    pub fn get_name(&self) -> &str {
        &self.name
    }

    pub fn get_regex(&self) -> &Regex {
        &self.regex
    }

    pub fn get_behavior(&self) -> &TokenBehavior {
        &self.behavior
    }

    pub fn get_priority(&self) -> i32 {
        self.priority
    }

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
