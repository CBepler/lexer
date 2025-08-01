use std::collections::{HashMap, HashSet};

use crate::Language;

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub enum Symbol {
    Terminal(String),
    NonTerminal(String),
}

#[derive(Clone)]
pub struct ProductionRule {
    head: String,
    body: Vec<Symbol>,
}

pub struct AmbiguousToken {
    base_token: String,
    token_variations: Vec<String>,
}

#[derive(Debug)]
pub struct Grammar {
    production_rules: HashMap<String, Vec<Vec<Symbol>>>,
    reverse_production_rule: HashMap<Vec<Symbol>, String>,
    start_symbol: String,
    valid_terminal_set: HashSet<String>,
    ambiguous_tokens: HashMap<String, Vec<String>>,
}

impl Grammar {
    pub fn new(
        start_symbol: String,
        valid_token_names: Vec<String>,
        production_rules: Vec<ProductionRule>,
        ambiguous_toks: Vec<AmbiguousToken>,
    ) -> Result<Self, String> {
        //Ensure no duplicate valid tokens
        let mut valid_terminal_set = HashSet::new();
        for tok_name in valid_token_names {
            if !valid_terminal_set.insert(tok_name.clone()) {
                return Err(format!("Duplicate Valid Token Name: {}", tok_name));
            }
        }
        //Ensure no duplicate Ambiguous tokens
        let mut ambiguous_map = HashMap::new();
        for ambiguous_tok in ambiguous_toks {
            if ambiguous_map
                .insert(
                    ambiguous_tok.base_token.clone(),
                    ambiguous_tok.token_variations,
                )
                .is_some()
            {
                return Err(format!(
                    "Duplicate Ambiguous Token Base name: {}",
                    ambiguous_tok.base_token
                ));
            }
        }
        //Remove base tokens from valid tokens and add their token variations
        for (ambiguous_tok, variations) in &ambiguous_map {
            valid_terminal_set.remove(ambiguous_tok);
            valid_terminal_set.extend(variations.clone().into_iter());
        }
        let valid_nonterminal_set: HashSet<String> = production_rules
            .iter()
            .map(|rule| rule.head.clone())
            .collect();
        //Ensure no non-terminals and terminals have the same name
        for terminal in &valid_terminal_set {
            if valid_nonterminal_set.contains(terminal) {
                return Err(format!(
                    "Terminal and nonterminal have the same name: {terminal}"
                ));
            }
        }
        let mut production_map = HashMap::new();
        let mut reverse_production_map = HashMap::new();
        for production_rule in production_rules {
            let head = production_rule.head;
            let body = production_rule.body;
            //Ensure all symbols used in production rules are valid
            for symbol in &body {
                match symbol {
                    Symbol::Terminal(x) => {
                        if !valid_terminal_set.contains(x) {
                            return Err(format!(
                                "Unrecognized terminal {x} used in production rule for terminal {head}"
                            ));
                        }
                    }
                    Symbol::NonTerminal(x) => {
                        if !valid_nonterminal_set.contains(x) {
                            return Err(format!(
                                "Unrecognized nonterminal {x} used in production rule for terminal {head}"
                            ));
                        }
                    }
                }
            }
            production_map
                .entry(head.clone())
                .or_insert_with(Vec::new)
                .push(body.clone());
            reverse_production_map.insert(body, head);
        }
        Ok(Grammar {
            start_symbol,
            production_rules: production_map,
            reverse_production_rule: reverse_production_map,
            valid_terminal_set,
            ambiguous_tokens: ambiguous_map,
        })
    }

    pub fn from_language(
        language: Language,
        production_rules: Vec<ProductionRule>,
        start_symbol: String,
        ambiguous_tokens: Vec<AmbiguousToken>,
    ) -> Result<Self, String> {
        let valid_token_names = language
            .get_token_definitions()
            .iter()
            .map(|def| def.name.clone())
            .collect();
        Grammar::new(
            start_symbol,
            valid_token_names,
            production_rules,
            ambiguous_tokens,
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::*;

    fn get_valid_grammar() -> Grammar {
        let start_symbol = String::from("S");
        let valid_token_names = vec![
            String::from("IDENTIFIER"),
            String::from("EQUALS"),
            String::from("NUMBER"),
        ];
        let production_rules = vec![
            ProductionRule {
                head: String::from("S"),
                body: vec![
                    Symbol::NonTerminal(String::from("A")),
                    Symbol::Terminal(String::from("EQUALS")),
                    Symbol::NonTerminal(String::from("B")),
                ],
            },
            ProductionRule {
                head: String::from("A"),
                body: vec![Symbol::Terminal(String::from("IDENTIFIER"))],
            },
            ProductionRule {
                head: String::from("B"),
                body: vec![Symbol::Terminal(String::from("NUMBER"))],
            },
        ];
        let ambiguous_tokens = vec![];

        let grammar = Grammar::new(
            start_symbol,
            valid_token_names,
            production_rules,
            ambiguous_tokens,
        );
        println!("Grammar: {:?}", grammar);
        grammar.unwrap()
    }

    #[test]
    fn test_valid_grammar_creation() {
        let grammar = get_valid_grammar();
        assert_eq!(grammar.start_symbol, "S");
        assert!(grammar.valid_terminal_set.contains("IDENTIFIER"));
        assert!(grammar.production_rules.contains_key("S"));
    }

    #[test]
    fn test_duplicate_valid_token_names() {
        let start_symbol = String::from("S");
        let valid_token_names = vec![
            String::from("IDENTIFIER"),
            String::from("IDENTIFIER"),
            String::from("EQUALS"),
        ];
        let production_rules = vec![];
        let ambiguous_tokens = vec![];

        let result = Grammar::new(
            start_symbol,
            valid_token_names,
            production_rules,
            ambiguous_tokens,
        );
        assert!(result.is_err());
        assert_eq!(
            result.unwrap_err(),
            "Duplicate Valid Token Name: IDENTIFIER"
        );
    }

    #[test]
    fn test_duplicate_ambiguous_token_base_name() {
        let start_symbol = String::from("S");
        let valid_token_names = vec![String::from("EQUALS"), String::from("NUMBER")];
        let production_rules = vec![];
        let ambiguous_tokens = vec![
            AmbiguousToken {
                base_token: String::from("IDENTIFIER"),
                token_variations: vec![String::from("VAR"), String::from("TYPE_DEF")],
            },
            AmbiguousToken {
                base_token: String::from("IDENTIFIER"),
                token_variations: vec![String::from("OTHER")],
            },
        ];

        let result = Grammar::new(
            start_symbol,
            valid_token_names,
            production_rules,
            ambiguous_tokens,
        );
        assert!(result.is_err());
        assert_eq!(
            result.unwrap_err(),
            "Duplicate Ambiguous Token Base name: IDENTIFIER"
        );
    }

    #[test]
    fn test_unrecognized_nonterminal() {
        let start_symbol = String::from("S");
        let valid_token_names = vec![String::from("IDENTIFIER")];
        let production_rules = vec![ProductionRule {
            head: String::from("S"),
            body: vec![Symbol::NonTerminal(String::from("A"))],
        }];
        let ambiguous_tokens = vec![];

        let result = Grammar::new(
            start_symbol,
            valid_token_names,
            production_rules,
            ambiguous_tokens,
        );
        assert!(result.is_err());
        assert_eq!(
            result.unwrap_err(),
            "Unrecognized nonterminal A used in production rule for terminal S"
        );
    }

    #[test]
    fn test_unrecognized_terminal() {
        let start_symbol = String::from("S");
        let valid_token_names = vec![String::from("IDENTIFIER")];
        let production_rules = vec![ProductionRule {
            head: String::from("S"),
            body: vec![Symbol::Terminal(String::from("EQUALS"))],
        }];
        let ambiguous_tokens = vec![];

        let result = Grammar::new(
            start_symbol,
            valid_token_names,
            production_rules,
            ambiguous_tokens,
        );
        assert!(result.is_err());
        assert_eq!(
            result.unwrap_err(),
            "Unrecognized terminal EQUALS used in production rule for terminal S"
        );
    }

    #[test]
    fn test_terminal_and_nonterminal_name_collision() {
        let start_symbol = String::from("S");
        let valid_token_names = vec![String::from("EQUALS"), String::from("S")];
        let production_rules = vec![ProductionRule {
            head: String::from("S"),
            body: vec![Symbol::Terminal(String::from("EQUALS"))],
        }];
        let ambiguous_tokens = vec![];

        let result = Grammar::new(
            start_symbol,
            valid_token_names,
            production_rules,
            ambiguous_tokens,
        );
        assert!(result.is_err());
        assert_eq!(
            result.unwrap_err(),
            "Terminal and nonterminal have the same name: S"
        );
    }

    #[test]
    fn test_valid_ambiguous_grammar_creation() {
        let start_symbol = String::from("S");
        let valid_token_names = vec![
            String::from("EQUALS"),
            String::from("NUMBER"),
            String::from("VAR"),
        ];
        let production_rules = vec![
            ProductionRule {
                head: String::from("S"),
                body: vec![
                    Symbol::NonTerminal(String::from("A")),
                    Symbol::Terminal(String::from("EQUALS")),
                    Symbol::Terminal(String::from("NUMBER")),
                ],
            },
            ProductionRule {
                head: String::from("A"),
                body: vec![
                    Symbol::Terminal(String::from("TYPE_DEF")),
                    Symbol::Terminal(String::from("VAR")),
                ],
            },
        ];
        let ambiguous_tokens = vec![AmbiguousToken {
            base_token: String::from("IDENTIFIER"),
            token_variations: vec![String::from("TYPE_DEF"), String::from("VAR")],
        }];

        let result = Grammar::new(
            start_symbol,
            valid_token_names,
            production_rules,
            ambiguous_tokens,
        );
        println!("Grammar: {:?}", result);
        assert!(result.is_ok());
        let grammar = result.unwrap();

        assert_eq!(grammar.ambiguous_tokens.get("IDENTIFIER").unwrap().len(), 2);
        assert!(
            grammar
                .ambiguous_tokens
                .get("IDENTIFIER")
                .unwrap()
                .contains(&String::from("TYPE_DEF"))
        );

        assert!(!grammar.valid_terminal_set.contains("IDENTIFIER"));

        assert!(grammar.valid_terminal_set.contains("TYPE_DEF"));
    }

    #[test]
    fn test_from_language() {
        let language_result = define_language! {
            ignore_token!("WHITESPACE", r"\s+", 10),

            keyword!("PLUS", r"\+", 80),
            keyword!("MINUS", r"-", 80),
        };
        let language = language_result.unwrap();
        let start_symbol = String::from("E");
        let production_rules = vec![
            ProductionRule {
                head: String::from("E"),
                body: vec![
                    Symbol::NonTerminal(String::from("T")),
                    Symbol::Terminal(String::from("PLUS")),
                    Symbol::NonTerminal(String::from("E")),
                ],
            },
            ProductionRule {
                head: String::from("T"),
                body: vec![Symbol::Terminal(String::from("MINUS"))],
            },
        ];
        let ambiguous_tokens = vec![];

        let result =
            Grammar::from_language(language, production_rules, start_symbol, ambiguous_tokens);
        assert!(result.is_ok());
        let grammar = result.unwrap();

        assert_eq!(grammar.start_symbol, "E");
        assert!(grammar.valid_terminal_set.contains("PLUS"));
        assert!(grammar.valid_terminal_set.contains("MINUS"));
        assert!(grammar.production_rules.contains_key("E"));
        assert!(grammar.production_rules.contains_key("T"));
    }
}
