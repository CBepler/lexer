use std::collections::{HashMap, HashSet};

use crate::{
    dfa::LexerDFA,
    language::{Language, PairDirection, TokenBehavior},
    nfa::LexerNFA,
    regex::escapes::EscapeChar,
};

type StateId = usize;

#[derive(Debug, PartialEq)]
pub struct Token {
    name: String,
    text_match: Option<String>,
}

impl Token {
    pub fn get_name(&self) -> &str {
        &self.name
    }

    pub fn get_match(&self) -> &Option<String> {
        &self.text_match
    }
}

pub struct Lexer {
    language_dfa: LexerDFA,
    //Map with name of ignore_until token to compile DFA for end match
    ignore_dfas: HashMap<String, LexerDFA>,
    open_pairs: HashMap<String, String>,
    close_pairs: HashMap<String, String>,
    ignores: HashSet<String>,
    to_store: HashMap<String, bool>,
}

impl Lexer {
    pub fn new(language: Language) -> Result<Self, String> {
        let mut patterns = Vec::new();
        let mut ignore_dfas = HashMap::new();
        let mut open_pairs = HashMap::new();
        let mut close_pairs = HashMap::new();
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
                        open_pairs.insert(
                            token_def.name.to_string(),
                            def.get_counterpart_name().to_string(),
                        );
                        close_pairs.insert(
                            def.get_counterpart_name().to_string(),
                            token_def.name.to_string(),
                        );
                    };
                }
                TokenBehavior::IgnoreUntilCompiled(reg) => {
                    let ignore_nfa = LexerNFA::new(vec![("END".to_string(), 1, reg)])?;
                    let ignore_dfa = LexerDFA::new(ignore_nfa)?;
                    ignore_dfas.insert(token_def.name.to_string(), ignore_dfa);
                }
                TokenBehavior::None => {}
                TokenBehavior::IgnoreUntil(_) => {
                    return Err(format!(
                        "Uncompiled IgnoreUntil behavior for token: {}",
                        token_def.name
                    ));
                }
            }
        }
        let nfa = LexerNFA::new(patterns)?;
        let language_dfa = LexerDFA::new(nfa)?;
        println!("Language DFA: {:?}", language_dfa);
        println!("ignore DFAs: {:?}", ignore_dfas);
        println!("open_pairs: {:?}", open_pairs);
        println!("close_pairs: {:?}", close_pairs);
        println!("ignores: {:?}", ignores);
        println!("to_store: {:?}", to_store);
        Ok(Lexer {
            language_dfa,
            ignore_dfas,
            open_pairs,
            close_pairs,
            ignores,
            to_store,
        })
    }

    pub fn lex(&self, text: &str) -> Result<Vec<Token>, String> {
        let mut tokens = Vec::new();
        let mut current_pos: usize = 0;
        let mut unclosed_pairs: HashMap<String, u32> = HashMap::new();

        while current_pos < text.len() {
            let remaining_text = &text[current_pos..];
            let mut current_dfa_state = self.language_dfa.get_start_state();
            let mut last_accepted_len: Option<usize> = None;
            let mut last_accepted_token_name: Option<String> = None;
            let mut current_lookahead_len: usize = 0;

            let prev_char_opt = if current_pos > 0 {
                text[..current_pos].chars().last()
            } else {
                None
            };
            let mut is_start_of_line = current_pos == 0 || check_line_end_char(prev_char_opt);

            let mut char_iter = remaining_text.chars().peekable();
            let mut consumed_chars_for_lookahead: Vec<char> = Vec::new();

            loop {
                println!("Current DFA state: {:?}", current_dfa_state);
                let char_at_lookahead_pos_opt = char_iter.peek().copied();

                let transitions_op = self.language_dfa.get_state_transitions(current_dfa_state);
                let transitions = if let Some(t) = transitions_op {
                    t
                } else {
                    break;
                };

                //Start Anchor Assertion (^)
                if let Some(target_state) = transitions.get_start_anchor_assertion_target() {
                    if is_start_of_line {
                        current_dfa_state = *target_state;
                        is_start_of_line = false;
                        if let Some(token_name) = self
                            .language_dfa
                            .get_accept_states()
                            .get(&current_dfa_state)
                        {
                            last_accepted_len = Some(current_lookahead_len);
                            last_accepted_token_name = Some(token_name.clone());
                        }
                        println!("Taking Start Anchor");
                        continue;
                    }
                }

                //Word Boundary Assertion (\b)
                if let Some(target_state) = transitions.get_word_boundry_assertion_target() {
                    let actual_prev_char_for_boundary = consumed_chars_for_lookahead
                        .last()
                        .copied()
                        .or(prev_char_opt);

                    if is_word_boundary(actual_prev_char_for_boundary, char_at_lookahead_pos_opt) {
                        current_dfa_state = *target_state;
                        if let Some(token_name) = self
                            .language_dfa
                            .get_accept_states()
                            .get(&current_dfa_state)
                        {
                            last_accepted_len = Some(current_lookahead_len);
                            last_accepted_token_name = Some(token_name.clone());
                        }
                        println!("Taking Word Boundry");
                        continue;
                    }
                }

                //End Anchor Assertion ($)
                let is_at_end_of_line_or_text = char_at_lookahead_pos_opt.is_none()
                    || check_line_end_char(char_at_lookahead_pos_opt);
                if let Some(target_state) = transitions.get_end_anchor_assertion_target() {
                    if is_at_end_of_line_or_text {
                        if self
                            .language_dfa
                            .get_accept_states()
                            .contains_key(target_state)
                        {
                            last_accepted_len = Some(current_lookahead_len);
                            last_accepted_token_name = self
                                .language_dfa
                                .get_accept_states()
                                .get(target_state)
                                .cloned();
                        }
                    }
                }

                //Character Transition
                let ch_to_consume_opt = char_iter.next();
                if ch_to_consume_opt.is_none() {
                    break;
                }
                let ch_to_consume = ch_to_consume_opt.unwrap();
                let ch_len = ch_to_consume.len_utf8();

                consumed_chars_for_lookahead.push(ch_to_consume);
                current_lookahead_len += ch_len;

                println!(
                    "Current transitions: {:?}",
                    transitions.get_range_transitions()
                );

                let next_range_state_id_opt =
                    next_range_state(ch_to_consume, transitions.get_range_transitions());

                println!("Next Range: {:?}", next_range_state_id_opt);

                if let Some(next_state) = next_range_state_id_opt {
                    current_dfa_state = next_state;
                    is_start_of_line = check_line_end_char(ch_to_consume_opt);

                    println!("Next DFA state: {:?}", current_dfa_state);
                    if let Some(token_name) = self
                        .language_dfa
                        .get_accept_states()
                        .get(&current_dfa_state)
                    {
                        last_accepted_len = Some(current_lookahead_len);
                        last_accepted_token_name = Some(token_name.clone());
                    }
                    println!("Last Accepted Token: {:?}", last_accepted_token_name);
                } else {
                    break;
                }
            }

            println!("Accepted Token: {:?}", last_accepted_token_name);

            // --- Finalize Token ---
            if let Some(matched_len) = last_accepted_len {
                let token_name = last_accepted_token_name.unwrap();
                let matched_text = &remaining_text[..matched_len];
                println!("Token match: {:?}", matched_text);

                if let Some(ignore_dfa) = self.ignore_dfas.get(&token_name) {
                    current_pos += matched_len;

                    let mut end_match_len: usize = 0;
                    let mut current_ignore_dfa_state = ignore_dfa.get_start_state();
                    let mut temp_ignore_lookahead_pos = current_pos;
                    let mut ignore_chars_iter = text[current_pos..].chars().peekable();

                    while let Some(ch_in_ignore) = ignore_chars_iter.next() {
                        let ch_in_ignore_len = ch_in_ignore.len_utf8();
                        let transitions =
                            ignore_dfa.get_state_transitions(current_ignore_dfa_state);

                        let mut next_state_for_ignore_char: Option<StateId> = None;
                        if let Some(trans_data) = transitions {
                            next_state_for_ignore_char =
                                next_range_state(ch_in_ignore, trans_data.get_range_transitions());
                        }

                        if let Some(next_state) = next_state_for_ignore_char {
                            current_ignore_dfa_state = next_state;
                        } else {
                            current_ignore_dfa_state = ignore_dfa.get_start_state();
                        }
                        temp_ignore_lookahead_pos += ch_in_ignore_len;

                        if ignore_dfa
                            .get_accept_states()
                            .contains_key(&current_ignore_dfa_state)
                        {
                            end_match_len = temp_ignore_lookahead_pos - current_pos;
                            break;
                        }
                    }

                    if end_match_len > 0 {
                        current_pos = temp_ignore_lookahead_pos;
                    } else {
                        return Err(format!(
                            "Unterminated ignore block starting with token '{}' at position {}",
                            token_name,
                            current_pos - matched_len
                        ));
                    }
                } else if !self.ignores.contains(&token_name) {
                    let text_to_store = if *self.to_store.get(&token_name).unwrap_or(&false) {
                        Some(matched_text.to_string())
                    } else {
                        None
                    };
                    if self.open_pairs.contains_key(&token_name) {
                        *unclosed_pairs.entry(token_name.clone()).or_insert(0) += 1;
                    } else if self.close_pairs.contains_key(&token_name) {
                        let num_open = *unclosed_pairs.entry(token_name.clone()).or_insert(0);
                        if num_open <= 0 {
                            return Err(format!(
                                "Unexpected closing pair found without matching open at position {}, '{}'",
                                current_pos, token_name,
                            ));
                        }
                        *unclosed_pairs.entry(token_name.clone()).or_insert(0) -= 1;
                    }
                    tokens.push(Token {
                        name: token_name,
                        text_match: text_to_store,
                    });
                    current_pos += matched_len;
                } else {
                    current_pos += matched_len;
                }
            } else {
                let char_at_error = text[current_pos..].chars().next().unwrap_or(' ');
                return Err(format!(
                    "Unexpected character at position {}: '{}'",
                    current_pos, char_at_error
                ));
            }
        }
        let entry = unclosed_pairs.iter().find(|entry| *entry.1 > 0);
        if let Some(unclosed) = entry {
            return Err(format!("Unclosed open pair: {}", unclosed.0));
        }
        Ok(tokens)
    }
}

fn check_line_end_char(c_opt: Option<char>) -> bool {
    matches!(c_opt, Some('\n') | Some('\r'))
}

fn is_word_char(c_opt: Option<char>) -> bool {
    match c_opt {
        Some(c) => EscapeChar::WordCharacter.matches_char(c),
        None => false,
    }
}

fn is_word_boundary(prev_char_opt: Option<char>, current_char_opt: Option<char>) -> bool {
    let prev_is_word = is_word_char(prev_char_opt);
    let current_is_word = is_word_char(current_char_opt);
    prev_is_word != current_is_word
}

fn next_range_state(ch: char, ranges: &Vec<((char, char), StateId)>) -> Option<StateId> {
    let result = ranges.binary_search_by(|((low, high), _target_state)| {
        if ch < *low {
            std::cmp::Ordering::Greater
        } else if ch > *high {
            std::cmp::Ordering::Less
        } else {
            std::cmp::Ordering::Equal
        }
    });

    match result {
        Ok(index) => Some(ranges[index].1),
        Err(_) => None,
    }
}

#[cfg(test)]
mod tests {
    use super::Lexer;
    use crate::language::Language;
    use crate::*;

    fn create_test_language() -> Result<Language, String> {
        define_language! {
            ignore_token!("WHITESPACE", r"\s+", 10),

            ignore_until!("SINGLE_LINE_COMMENT", r"//", r"\n", 20),
            ignore_until!("MULTI_LINE_COMMENT", r"/\*", r"\*/", 20),

            keyword!("VAR_KEYWORD", r"\bvar\b", 100),
            keyword!("FUNC_KEYWORD", r"\bfunc\b", 100),
            keyword!("RETURN_KEYWORD", r"\breturn\b", 100),

            token!("IDENTIFIER", r"[a-zA-Z_][a-zA-Z0-9_]*", 90, true),

            token!("INTEGER_LITERAL", r"\d+", 90, true),

            keyword!("ASSIGN", r"=", 70),
            keyword!("PLUS", r"\+", 70),
            keyword!("MINUS", r"-", 70),
            keyword!("MULTIPLY", r"\*", 70),
            keyword!("DIVIDE", r"/", 70),

            open_pair!("LEFT_PAREN", r"\(", "RIGHT_PAREN", 80),
            close_pair!("RIGHT_PAREN", r"\)", "LEFT_PAREN", 80),
            open_pair!("LEFT_BRACE", r"\{", "RIGHT_BRACE", 80),
            close_pair!("RIGHT_BRACE", r"\}", "LEFT_BRACE", 80),
            open_pair!("LEFT_BRACKET", r"\[", "RIGHT_BRACKET", 80),
            close_pair!("RIGHT_BRACKET", r"\]", "LEFT_BRACKET", 80),

            keyword!("SEMICOLON", r";", 70),
            keyword!("COMMA", r",", 70)
        }
    }

    #[test]
    fn lexer_basic_tokens_success() {
        let language = create_test_language().unwrap();
        let lexer = Lexer::new(language).unwrap();
        let input = "var myVar = 123;";
        let tokens = lexer.lex(input).unwrap();

        assert_eq!(tokens.len(), 5);
        assert_eq!(tokens[0].get_name(), "VAR_KEYWORD");
        assert_eq!(tokens[1].get_name(), "IDENTIFIER");
        assert_eq!(tokens[1].get_match(), &Some("myVar".to_string()));
        assert_eq!(tokens[2].get_name(), "ASSIGN");
        assert_eq!(tokens[3].get_name(), "INTEGER_LITERAL");
        assert_eq!(tokens[3].get_match(), &Some("123".to_string()));
        assert_eq!(tokens[4].get_name(), "SEMICOLON");
    }

    #[test]
    fn lexer_whitespace_and_comments() {
        let language = create_test_language().unwrap();
        let lexer = Lexer::new(language).unwrap();
        let input = r#"
            // This is a single-line comment
            var /* multi-line comment */ myOtherVar = 456; // Another comment
        "#;
        let tokens = lexer.lex(input).unwrap();

        assert_eq!(tokens.len(), 5);
        assert_eq!(tokens[0].get_name(), "VAR_KEYWORD");
        assert_eq!(tokens[1].get_name(), "IDENTIFIER");
        assert_eq!(tokens[1].get_match(), &Some("myOtherVar".to_string()));
        assert_eq!(tokens[2].get_name(), "ASSIGN");
        assert_eq!(tokens[3].get_name(), "INTEGER_LITERAL");
        assert_eq!(tokens[3].get_match(), &Some("456".to_string()));
        assert_eq!(tokens[4].get_name(), "SEMICOLON");
    }

    #[test]
    fn lexer_paired_tokens_success() {
        let language = create_test_language().unwrap();
        let lexer = Lexer::new(language).unwrap();
        let input = "func myFunc(arg1, arg2) { return 0; }";
        let tokens = lexer.lex(input).unwrap();

        assert_eq!(tokens.len(), 14);
        assert_eq!(tokens[0].get_name(), "FUNC_KEYWORD");
        assert_eq!(tokens[1].get_name(), "IDENTIFIER");
        assert_eq!(tokens[2].get_name(), "LEFT_PAREN");
        assert_eq!(tokens[3].get_name(), "IDENTIFIER");
        assert_eq!(tokens[4].get_name(), "COMMA");
        assert_eq!(tokens[5].get_name(), "IDENTIFIER");
        assert_eq!(tokens[6].get_name(), "RIGHT_PAREN");
        assert_eq!(tokens[7].get_name(), "LEFT_BRACE");
        assert_eq!(tokens[8].get_name(), "RETURN_KEYWORD");
        assert_eq!(tokens[9].get_name(), "INTEGER_LITERAL");
        assert_eq!(tokens[10].get_name(), "SEMICOLON");
        assert_eq!(tokens[11].get_name(), "RIGHT_BRACE");
    }

    #[test]
    fn lexer_unclosed_pair_error() {
        let language = create_test_language().unwrap();
        let lexer = Lexer::new(language).unwrap();
        let input = "func myFunc(arg1 { return 0; }"; // Missing closing paren
        let result = lexer.lex(input);

        assert!(result.is_err());
        assert!(
            result
                .unwrap_err()
                .contains("Unclosed open pair: LEFT_PAREN")
        );
    }

    #[test]
    fn lexer_unexpected_closing_pair_error() {
        let language = create_test_language().unwrap();
        let lexer = Lexer::new(language).unwrap();
        let input = "func myFunc) { return 0; }"; // Unexpected closing paren
        let result = lexer.lex(input);

        assert!(result.is_err());
        assert!(
            result
                .unwrap_err()
                .contains("Unexpected closing pair found without matching open at position")
        );
    }

    #[test]
    fn lexer_unterminated_multi_line_comment_error() {
        let language = create_test_language().unwrap();
        let lexer = Lexer::new(language).unwrap();
        let input = "var x = /* unterminated comment";
        let result = lexer.lex(input);

        assert!(result.is_err());
        assert!(
            result
                .unwrap_err()
                .contains("Unterminated ignore block starting with token 'MULTI_LINE_COMMENT'")
        );
    }

    #[test]
    fn lexer_unrecognized_character_error() {
        let language = create_test_language().unwrap();
        let lexer = Lexer::new(language).unwrap();
        let input = "var $x = 10;"; // '$' is not defined
        let result = lexer.lex(input);

        assert!(result.is_err());
        assert!(result.unwrap_err().contains("'$'"));
    }

    #[test]
    fn lexer_complex_expression() {
        let language = create_test_language().unwrap();
        let lexer = Lexer::new(language).unwrap();
        let input = "var result = (10 + myVar) * 2 / funcCall();";
        let tokens = lexer.lex(input).unwrap();

        let expected_names = vec![
            "VAR_KEYWORD",
            "IDENTIFIER",
            "ASSIGN",
            "LEFT_PAREN",
            "INTEGER_LITERAL",
            "PLUS",
            "IDENTIFIER",
            "RIGHT_PAREN",
            "MULTIPLY",
            "INTEGER_LITERAL",
            "DIVIDE",
            "IDENTIFIER",
            "LEFT_PAREN",
            "RIGHT_PAREN",
            "SEMICOLON",
        ];

        assert_eq!(tokens.len(), expected_names.len());
        for (i, expected_name) in expected_names.iter().enumerate() {
            assert_eq!(
                tokens[i].get_name(),
                *expected_name,
                "Token mismatch at index {}",
                i
            );
        }
        assert_eq!(tokens[1].get_match(), &Some("result".to_string()));
        assert_eq!(tokens[4].get_match(), &Some("10".to_string()));
        assert_eq!(tokens[6].get_match(), &Some("myVar".to_string()));
        assert_eq!(tokens[9].get_match(), &Some("2".to_string()));
        assert_eq!(tokens[11].get_match(), &Some("funcCall".to_string()));
    }

    #[test]
    fn lexer_word_boundary_behavior() {
        let language = create_test_language().unwrap();
        let lexer = Lexer::new(language).unwrap();

        let input1 = "varvariable";
        let tokens1 = lexer.lex(input1).unwrap();
        assert_eq!(tokens1.len(), 1);
        assert_eq!(tokens1[0].get_name(), "IDENTIFIER");

        let input2 = "var variable";
        let tokens2 = lexer.lex(input2).unwrap();
        assert_eq!(tokens2.len(), 2);
        assert_eq!(tokens2[0].get_name(), "VAR_KEYWORD");
        assert_eq!(tokens2[1].get_name(), "IDENTIFIER");
        assert_eq!(tokens2[1].get_match(), &Some("variable".to_string()));

        let input3 = "return_value";
        let tokens3 = lexer.lex(input3).unwrap();
        assert_eq!(tokens3.len(), 1);
        assert_eq!(tokens3[0].get_name(), "IDENTIFIER");

        let input4 = "return 1;";
        let tokens4 = lexer.lex(input4).unwrap();
        assert_eq!(tokens4.len(), 3);
        assert_eq!(tokens4[0].get_name(), "RETURN_KEYWORD");
        assert_eq!(tokens4[1].get_name(), "INTEGER_LITERAL");
        assert_eq!(tokens4[2].get_name(), "SEMICOLON");
    }
}
