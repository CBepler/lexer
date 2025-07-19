use regex::Regex;

use token_behavior::{TokenBehavior, comment_end_condition::CommentEndCondition};

pub mod token_behavior;

#[derive(Debug)]
pub struct TokenDefinition {
    name: String,
    regex: Regex,
    behavior: TokenBehavior,
}

impl TokenDefinition {
    pub fn new(name: String, regex_str: &str, behavior: TokenBehavior) -> Result<Self, String> {
        let compiled_regex = Regex::new(regex_str)
            .map_err(|e| format!("Invalid regex for token '{}': {}", name, e))?;

        let final_behavior = match behavior {
            TokenBehavior::CommentStart(CommentEndCondition::RegexStr(end_regex_str)) => {
                let compiled_end_regex = Regex::new(&end_regex_str)
                    .map_err(|e| format!("Invalid end regex for comment '{}': {}", name, e))?;
                TokenBehavior::CommentStart(CommentEndCondition::Regex(compiled_end_regex))
            }
            _ => behavior,
        };

        Ok(TokenDefinition {
            name,
            regex: compiled_regex,
            behavior: final_behavior,
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
}

impl PartialEq for TokenDefinition {
    fn eq(&self, other: &Self) -> bool {
        (self.name == other.name)
            && (self.regex.to_string() == other.regex.to_string())
            && (self.behavior == other.behavior)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn token_definition_new_correct_assert_eq() {
        let d1 = TokenDefinition::new("MOD".to_string(), r"mod", TokenBehavior::Keyword);
        let d2 = TokenDefinition::new("MOD".to_string(), r"mod", TokenBehavior::Keyword);
        assert_eq!(d1, d2);
    }

    #[test]
    fn token_definition_new_correct_regex_str_assert_eq() {
        let d1 = TokenDefinition::new(
            "MOD".to_string(),
            r"mod",
            TokenBehavior::CommentStart(CommentEndCondition::RegexStr(r"cow".to_string())),
        );
        let d2 = TokenDefinition::new(
            "MOD".to_string(),
            r"mod",
            TokenBehavior::CommentStart(CommentEndCondition::Regex(Regex::new(r"cow").unwrap())),
        );
        assert_eq!(d1, d2);
    }

    #[test]
    fn token_definition_new_invalid_regex_assert_err() {
        let d1 = TokenDefinition::new("MOD".to_string(), r"[", TokenBehavior::Keyword);
        assert!(d1.is_err());
    }

    #[test]
    fn token_definition_new_invalid_regexstr_regex_assert_err() {
        let d1 = TokenDefinition::new(
            "MOD".to_string(),
            r"mod",
            TokenBehavior::CommentStart(CommentEndCondition::RegexStr(r"[".to_string())),
        );
        assert!(d1.is_err());
    }
}
