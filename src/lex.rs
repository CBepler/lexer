use std::{
    collections::{HashMap, HashSet, VecDeque},
    hash::Hash,
};

use crate::{
    dfa::LexerDFA,
    language::{Language, PairDefinition, PairDirection, TokenBehavior},
    nfa::LexerNFA,
};

struct Token<'a> {
    name: String,
    text_match: Option<String>,
    pair: Option<&'a Token<'a>>,
}

struct lexer {
    language_dfa: LexerDFA,
    //Map with name of ignore_until token to compile DFA for end match
    ignore_dfas: HashMap<String, LexerDFA>,
    pairs: HashMap<String, String>,
    ignores: HashSet<String>,
    to_store: HashMap<String, bool>,
}

impl lexer {
    pub fn new(language: Language) -> Result<Self, String> {
        let mut patterns = Vec::new();
        let mut ignore_dfas = HashMap::new();
        let mut pairs = HashMap::new();
        let mut ignores = HashSet::new();
        let mut to_store = HashMap::new();
        for token_def in language.token_definitions {
            patterns.push((
                token_def.get_name().to_string(),
                token_def.get_priority(),
                token_def.regex,
            ));
            to_store.insert(token_def.name.to_string(), token_def.to_store_match);
            match token_def.behavior {
                TokenBehavior::Ignore => {
                    ignores.insert(token_def.name.to_string());
                }
                TokenBehavior::Pair(def) => {
                    if *def.get_pair_type() == PairDirection::Open {
                        pairs.insert(
                            token_def.name.to_string(),
                            def.get_counterpart_name().to_string(),
                        );
                    };
                }
                TokenBehavior::IgnoreUntilCompiled(reg) => {
                    let ignore_nfa = LexerNFA::new(vec![("END".to_string(), 1, reg)])?;
                    let ignore_dfa = LexerDFA::new(ignore_nfa);
                    ignore_dfas.insert(token_def.name.to_string(), ignore_dfa);
                }
                _ => (),
            }
        }
        let nfa = LexerNFA::new(patterns)?;
        let language_dfa = LexerDFA::new(nfa);
        Ok(lexer {
            language_dfa,
            ignore_dfas,
            pairs,
            ignores,
            to_store,
        })
    }

    pub fn lex(&self, text: &str) -> Result<Vec<Token>, String> {
        //map from open token name to vecdeque of currently awaiting open tokens for close
        let mut open_pair_map: HashMap<String, VecDeque<&Token>> = HashMap::new();
        let mut tokens = Vec::new();
        let mut current_state = self.language_dfa.get_start_state();
        let mut current_dfa = &self.language_dfa;
        while let Some(ch) = text.chars().next() {
            let transitions = current_dfa.get_state_transitions(current_state);
        }
        Ok(tokens)
    }
}
