use regex::Regex;
use std::collections::HashSet;

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
            match &tok.behavior {
                TokenBehavior::Pair(pair_def) => match pair_def.pair_type {
                    PairDirection::Open => {
                        open_pair_names.insert(tok.name.clone());
                        expected_pairs
                            .insert((tok.name.clone(), pair_def.counterpart_name.clone()));
                    }
                    PairDirection::Close => {
                        close_pair_names.insert(tok.name.clone());
                        expected_pairs
                            .insert((tok.name.clone(), pair_def.counterpart_name.clone()));
                    }
                },
                _ => (),
            };
            if !token_names.insert(tok.name.clone()) {
                return Err(format!("Duplicate token name found: '{}'", tok.name));
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

pub enum TokenBehavior {
    Ignore,
    Keyword,
    Pair(PairDefinition),
    Store,
    CommentStart(CommentEndCondition),
}

pub struct PairDefinition {
    pair_type: PairDirection,
    counterpart_name: String,
}

impl PairDefinition {
    pub fn new(pair_type: PairDirection, counterpart_name: String) -> Self {
        PairDefinition {
            pair_type,
            counterpart_name,
        }
    }

    pub fn get_pair_type(&self) -> &PairDirection {
        &self.pair_type
    }

    pub fn get_counterpart_name(&self) -> &str {
        &self.counterpart_name
    }
}

pub enum PairDirection {
    Open,
    Close,
}

pub enum CommentEndCondition {
    Newline,
    RegexStr(String),
    Regex(Regex),
}
