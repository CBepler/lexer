use std::{
    collections::{HashMap, HashSet},
    fmt::{self, Display},
};

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
    row: usize,
    col: usize,
}

impl Token {
    pub fn get_name(&self) -> &str {
        &self.name
    }

    pub fn get_match(&self) -> &Option<String> {
        &self.text_match
    }

    pub fn get_row(&self) -> usize {
        self.row
    }

    pub fn get_col(&self) -> usize {
        self.col
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(text) = &self.text_match {
            write!(f, "{}({})", self.name, text)
        } else {
            write!(f, "{}", self.name)
        }
    }
}

#[derive(Debug, Clone)]
struct MatchState {
    dfa_state: StateId,
    matched_len: usize,
    is_start_of_line_next: bool,
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
        let mut current_row: usize = 1;
        let mut current_col: usize = 1;
        let mut current_pos: usize = 0;
        let mut unclosed_pairs: HashMap<String, u32> = HashMap::new();

        while current_pos < text.len() {
            let remaining_text = &text[current_pos..];
            let mut active_match_states: Vec<MatchState> = Vec::new();

            let initial_dfa_state = self.language_dfa.get_start_state();
            active_match_states.push(MatchState {
                dfa_state: initial_dfa_state,
                matched_len: 0,
                is_start_of_line_next: current_pos == 0
                    || check_line_end_char(text[..current_pos].chars().last()),
            });

            let mut best_match_len: usize = 0;
            let mut best_match_token_name: Option<String> = None;

            let mut char_iter = remaining_text.chars().peekable();
            let mut consumed_chars_for_lookahead: Vec<char> = Vec::new();

            loop {
                let char_at_lookahead_pos_opt = char_iter.peek().copied();

                let mut next_active_match_states: Vec<MatchState> = Vec::new();

                let mut possible_consume_accepts = Vec::new();
                let mut possible_no_consume_accepts = Vec::new();

                loop {
                    let match_state = match active_match_states.pop() {
                        Some(state) => state,
                        None => break,
                    };
                    let transitions = self
                        .language_dfa
                        .get_state_transitions(match_state.dfa_state);

                    let transitions = if let Some(t) = transitions {
                        t
                    } else {
                        continue;
                    };

                    println!("Match State: {:?}", match_state);
                    println!("Transitions: {:?}", transitions);

                    if let Some(target_state) = transitions.get_start_anchor_assertion_target() {
                        if match_state.is_start_of_line_next {
                            let new_state = MatchState {
                                dfa_state: *target_state,
                                matched_len: match_state.matched_len,
                                is_start_of_line_next: false,
                            };
                            active_match_states.push(new_state.clone());
                        }
                    }

                    let prev_char_opt_for_boundary = consumed_chars_for_lookahead
                        .last()
                        .copied()
                        .or_else(|| text[..current_pos].chars().last());

                    if let Some(target_state) = transitions.get_word_boundry_assertion_target() {
                        if is_word_boundary(prev_char_opt_for_boundary, char_at_lookahead_pos_opt) {
                            println!("Word Boundry Added");
                            let new_state = MatchState {
                                dfa_state: *target_state,
                                matched_len: match_state.matched_len,
                                is_start_of_line_next: match_state.is_start_of_line_next,
                            };
                            active_match_states.push(new_state.clone());
                        }
                    }

                    let is_at_end_of_line_or_text = char_at_lookahead_pos_opt.is_none()
                        || check_line_end_char(char_at_lookahead_pos_opt);
                    if let Some(target_state) = transitions.get_end_anchor_assertion_target() {
                        if is_at_end_of_line_or_text {
                            let new_state = MatchState {
                                dfa_state: *target_state,
                                matched_len: match_state.matched_len,
                                is_start_of_line_next: is_at_end_of_line_or_text,
                            };
                            active_match_states.push(new_state.clone());
                        }
                    }

                    if let Some(ch_to_consume) = char_at_lookahead_pos_opt {
                        let next_range_state_id_opt =
                            next_range_state(ch_to_consume, transitions.get_range_transitions());
                        println!("Next Range: {:?}", next_range_state_id_opt);
                        if let Some(next_dfa_state) = next_range_state_id_opt {
                            let ch_len = ch_to_consume.len_utf8();
                            let new_matched_len = match_state.matched_len + ch_len;
                            let new_is_start_of_line_next =
                                check_line_end_char(Some(ch_to_consume));
                            let accept_name = self
                                .language_dfa
                                .get_accept_states()
                                .get(&next_dfa_state)
                                .cloned();
                            if accept_name.is_some() {
                                possible_consume_accepts.push(accept_name.unwrap());
                            }
                            let new_state = MatchState {
                                dfa_state: next_dfa_state,
                                matched_len: new_matched_len,
                                is_start_of_line_next: new_is_start_of_line_next,
                            };
                            next_active_match_states.push(new_state);
                        } else {
                            let accept_name = self
                                .language_dfa
                                .get_accept_states()
                                .get(&match_state.dfa_state)
                                .cloned();
                            if accept_name.is_some() {
                                possible_no_consume_accepts.push(accept_name.unwrap());
                            }
                        }
                    }
                }

                active_match_states = next_active_match_states;

                println!("Active match states: {:?}", active_match_states);

                let viable_accepts = match possible_consume_accepts.is_empty() {
                    true => possible_no_consume_accepts,
                    false => {
                        best_match_len += 1;
                        best_match_token_name = Some(possible_consume_accepts.remove(0));
                        possible_consume_accepts
                    }
                };

                println!("Viable Accepts: {:?}", viable_accepts);

                for accept in viable_accepts {
                    let current_priority =
                        self.language_dfa.get_token_priority(&accept).unwrap_or(&0);

                    best_match_token_name = match best_match_token_name {
                        Some(prev_best) => {
                            let best_priority = self
                                .language_dfa
                                .get_token_priority(&prev_best)
                                .unwrap_or(&0);
                            let best_token = match current_priority > best_priority {
                                true => accept,
                                false => prev_best,
                            };
                            Some(best_token)
                        }
                        None => Some(accept),
                    };
                }
                println!(
                    "Best match so far: {:?} at length {:?}",
                    best_match_token_name, best_match_len
                );

                if let Some(ch_to_consume) = char_iter.next() {
                    consumed_chars_for_lookahead.push(ch_to_consume);

                    if active_match_states.is_empty() {
                        break;
                    }
                } else {
                    break;
                }
            }

            println!("Accepted Token: {:?}", best_match_token_name);
            println!("Unclosed Pairs: {:?}", unclosed_pairs);

            // --- Finalize Token ---
            if best_match_len > 0 {
                let matched_len = best_match_len;
                let token_name = best_match_token_name.unwrap();
                let matched_text = &remaining_text[..matched_len];
                println!("Token match: {:?}", matched_text);
                let initial_token_row = current_row;
                let initial_token_col = current_col;
                for ch in matched_text.chars() {
                    (current_row, current_col) = update_row_col(ch, current_row, current_col)
                }

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
                            if let Some(trans_data_start) =
                                ignore_dfa.get_state_transitions(ignore_dfa.get_start_state())
                            {
                                if let Some(next_start_state) = next_range_state(
                                    ch_in_ignore,
                                    trans_data_start.get_range_transitions(),
                                ) {
                                    current_ignore_dfa_state = next_start_state;
                                }
                            }
                        }

                        (current_row, current_col) =
                            update_row_col(ch_in_ignore, current_row, current_col);

                        temp_ignore_lookahead_pos += ch_in_ignore_len;

                        if ignore_dfa
                            .get_accept_states()
                            .contains_key(&current_ignore_dfa_state)
                        {
                            end_match_len = temp_ignore_lookahead_pos - current_pos;
                            break;
                        }
                    }

                    println!("End Match length: {:?}", end_match_len);

                    if end_match_len > 0 {
                        current_pos = temp_ignore_lookahead_pos;
                    } else {
                        return Err(format!(
                            "Unterminated ignore block starting with token '{}' at row: {}  col: {}",
                            token_name, initial_token_row, initial_token_col
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
                        let num_open = *unclosed_pairs
                            .entry(self.close_pairs.get(&token_name).unwrap().clone())
                            .or_insert(0);
                        if num_open <= 0 {
                            return Err(format!(
                                "Unexpected closing pair found without matching open at row: {} col: {} : '{}'",
                                initial_token_row, initial_token_col, token_name,
                            ));
                        }
                        *unclosed_pairs
                            .entry(self.close_pairs.get(&token_name).unwrap().clone())
                            .or_insert(0) -= 1;
                    }
                    tokens.push(Token {
                        name: token_name,
                        text_match: text_to_store,
                        row: initial_token_row,
                        col: initial_token_col,
                    });
                    current_pos += matched_len;
                } else {
                    current_pos += matched_len;
                }
            } else {
                let char_at_error = text[current_pos..].chars().next().unwrap_or(' ');
                return Err(format!(
                    "Unexpected character at row {} col {}: '{}'",
                    current_row, current_col, char_at_error
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
    println!("char: {:?}", ch);
    println!("Ranges: {:?}", ranges);
    let result = ranges.binary_search_by(|((low, high), _target_state)| {
        if ch < *low {
            std::cmp::Ordering::Greater
        } else if ch > *high {
            std::cmp::Ordering::Less
        } else {
            std::cmp::Ordering::Equal
        }
    });

    println!("Result: {:?}", result);

    match result {
        Ok(index) => Some(ranges[index].1),
        Err(_) => None,
    }
}

fn update_row_col(ch: char, row: usize, col: usize) -> (usize, usize) {
    if ch == '\n' {
        (row + 1, 1)
    } else if ch == '\r' {
        (row, col)
    } else {
        (row, col + 1)
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

        println!("Tokens: {:?}", tokens);

        assert_eq!(tokens.len(), 5);
        assert_eq!(tokens[0].get_name(), "VAR_KEYWORD");
        assert_eq!(tokens[1].get_name(), "IDENTIFIER");
        assert_eq!(tokens[1].get_match(), &Some("myVar".to_string()));
        assert_eq!(tokens[2].get_name(), "ASSIGN");
        assert_eq!(tokens[3].get_name(), "INTEGER_LITERAL");
        assert_eq!(tokens[3].get_match(), &Some("123".to_string()));
        assert_eq!(tokens[4].get_name(), "SEMICOLON");

        assert_eq!(tokens[0].get_row(), 1);
        assert_eq!(tokens[0].get_col(), 1);
        assert_eq!(tokens[1].get_row(), 1);
        assert_eq!(tokens[1].get_col(), 5);
        assert_eq!(tokens[2].get_row(), 1);
        assert_eq!(tokens[2].get_col(), 11);
        assert_eq!(tokens[3].get_row(), 1);
        assert_eq!(tokens[3].get_col(), 13);
        assert_eq!(tokens[4].get_row(), 1);
        assert_eq!(tokens[4].get_col(), 16);
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

        assert_eq!(tokens[0].get_name(), "VAR_KEYWORD");
        assert_eq!(tokens[0].get_row(), 3);
        assert_eq!(tokens[0].get_col(), 13);

        assert_eq!(tokens[1].get_name(), "IDENTIFIER");
        assert_eq!(tokens[1].get_row(), 3);
        assert_eq!(tokens[1].get_col(), 42);

        assert_eq!(tokens[2].get_name(), "ASSIGN");
        assert_eq!(tokens[2].get_row(), 3);
        assert_eq!(tokens[2].get_col(), 53);

        assert_eq!(tokens[3].get_name(), "INTEGER_LITERAL");
        assert_eq!(tokens[3].get_row(), 3);
        assert_eq!(tokens[3].get_col(), 55);

        assert_eq!(tokens[4].get_name(), "SEMICOLON");
        assert_eq!(tokens[4].get_row(), 3);
        assert_eq!(tokens[4].get_col(), 58);
    }

    #[test]
    fn lexer_paired_tokens_success() {
        let language = create_test_language().unwrap();
        let lexer = Lexer::new(language).unwrap();
        let input = "func myFunc(arg1, arg2) { return 0; }";
        let tokens = lexer.lex(input).unwrap();

        println!("Tokens: {:?}", tokens);

        assert_eq!(tokens.len(), 12);
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

        assert_eq!(tokens[2].get_name(), "LEFT_PAREN");
        assert_eq!(tokens[2].get_row(), 1);
        assert_eq!(tokens[2].get_col(), 12);

        assert_eq!(tokens[6].get_name(), "RIGHT_PAREN");
        assert_eq!(tokens[6].get_row(), 1);
        assert_eq!(tokens[6].get_col(), 23);

        assert_eq!(tokens[7].get_name(), "LEFT_BRACE");
        assert_eq!(tokens[7].get_row(), 1);
        assert_eq!(tokens[7].get_col(), 25);

        assert_eq!(tokens[11].get_name(), "RIGHT_BRACE");
        assert_eq!(tokens[11].get_row(), 1);
        assert_eq!(tokens[11].get_col(), 37);
    }

    #[test]
    fn lexer_unclosed_pair_error() {
        let language = create_test_language().unwrap();
        let lexer = Lexer::new(language).unwrap();
        let input = "func myFunc(arg1 { return 0; }";
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
        let input = "func myFunc) { return 0; }";
        let result = lexer.lex(input);

        assert!(result.is_err());
        assert!(result.unwrap_err().contains("Unexpected closing pair"));
    }

    #[test]
    fn lexer_unterminated_multi_line_comment_error() {
        let language = create_test_language().unwrap();
        let lexer = Lexer::new(language).unwrap();
        let input = "var x = /* unterminated comment";
        let result = lexer.lex(input);

        assert!(result.is_err());
        assert!(result.unwrap_err().contains(
            "Unterminated ignore block starting with token 'MULTI_LINE_COMMENT' at row: 1  col: 9"
        ));
    }

    #[test]
    fn lexer_unrecognized_character_error() {
        let language = create_test_language().unwrap();
        let lexer = Lexer::new(language).unwrap();
        let input = "var $x = 10;";
        let result = lexer.lex(input);

        assert!(result.is_err());
        assert!(
            result
                .unwrap_err()
                .contains("Unexpected character at row 1 col 5: '$'")
        );
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

        assert_eq!(tokens[0].get_row(), 1);
        assert_eq!(tokens[0].get_col(), 1);
        assert_eq!(tokens[1].get_row(), 1);
        assert_eq!(tokens[1].get_col(), 5);
        assert_eq!(tokens[2].get_row(), 1);
        assert_eq!(tokens[2].get_col(), 12);
        assert_eq!(tokens[3].get_row(), 1);
        assert_eq!(tokens[3].get_col(), 14);
        assert_eq!(tokens[4].get_row(), 1);
        assert_eq!(tokens[4].get_col(), 15);
        assert_eq!(tokens[5].get_row(), 1);
        assert_eq!(tokens[5].get_col(), 18);
        assert_eq!(tokens[6].get_row(), 1);
        assert_eq!(tokens[6].get_col(), 20);
        assert_eq!(tokens[7].get_row(), 1);
        assert_eq!(tokens[7].get_col(), 25);
        assert_eq!(tokens[8].get_row(), 1);
        assert_eq!(tokens[8].get_col(), 27);
        assert_eq!(tokens[9].get_row(), 1);
        assert_eq!(tokens[9].get_col(), 29);
        assert_eq!(tokens[10].get_row(), 1);
        assert_eq!(tokens[10].get_col(), 31);
        assert_eq!(tokens[11].get_row(), 1);
        assert_eq!(tokens[11].get_col(), 33);
        assert_eq!(tokens[12].get_row(), 1);
        assert_eq!(tokens[12].get_col(), 41);
        assert_eq!(tokens[13].get_row(), 1);
        assert_eq!(tokens[13].get_col(), 42);
        assert_eq!(tokens[14].get_row(), 1);
        assert_eq!(tokens[14].get_col(), 43);
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

    #[test]
    fn test_row_col_simple_newline() {
        let language = create_test_language().unwrap();
        let lexer = Lexer::new(language).unwrap();
        let input = "var x;\ny = 10;";
        let tokens = lexer.lex(input).unwrap();

        assert_eq!(tokens[0].get_name(), "VAR_KEYWORD");
        assert_eq!(tokens[0].get_row(), 1);
        assert_eq!(tokens[0].get_col(), 1);

        assert_eq!(tokens[1].get_name(), "IDENTIFIER");
        assert_eq!(tokens[1].get_row(), 1);
        assert_eq!(tokens[1].get_col(), 5);

        assert_eq!(tokens[2].get_name(), "SEMICOLON");
        assert_eq!(tokens[2].get_row(), 1);
        assert_eq!(tokens[2].get_col(), 6);

        assert_eq!(tokens[3].get_name(), "IDENTIFIER");
        assert_eq!(tokens[3].get_row(), 2);
        assert_eq!(tokens[3].get_col(), 1);

        assert_eq!(tokens[4].get_name(), "ASSIGN");
        assert_eq!(tokens[4].get_row(), 2);
        assert_eq!(tokens[4].get_col(), 3);

        assert_eq!(tokens[5].get_name(), "INTEGER_LITERAL");
        assert_eq!(tokens[5].get_row(), 2);
        assert_eq!(tokens[5].get_col(), 5);

        assert_eq!(tokens[6].get_name(), "SEMICOLON");
        assert_eq!(tokens[6].get_row(), 2);
        assert_eq!(tokens[6].get_col(), 7);
    }

    #[test]
    fn test_row_col_multiple_newlines() {
        let language = create_test_language().unwrap();
        let lexer = Lexer::new(language).unwrap();
        let input = "func a()\n\nvar b;";
        let tokens = lexer.lex(input).unwrap();

        assert_eq!(tokens[0].get_name(), "FUNC_KEYWORD");
        assert_eq!(tokens[0].get_row(), 1);
        assert_eq!(tokens[0].get_col(), 1);

        assert_eq!(tokens[1].get_name(), "IDENTIFIER");
        assert_eq!(tokens[1].get_row(), 1);
        assert_eq!(tokens[1].get_col(), 6);

        assert_eq!(tokens[2].get_name(), "LEFT_PAREN");
        assert_eq!(tokens[2].get_row(), 1);
        assert_eq!(tokens[2].get_col(), 7);

        assert_eq!(tokens[3].get_name(), "RIGHT_PAREN");
        assert_eq!(tokens[3].get_row(), 1);
        assert_eq!(tokens[3].get_col(), 8);

        assert_eq!(tokens[4].get_name(), "VAR_KEYWORD");
        assert_eq!(tokens[4].get_row(), 3);
        assert_eq!(tokens[4].get_col(), 1);

        assert_eq!(tokens[5].get_name(), "IDENTIFIER");
        assert_eq!(tokens[5].get_row(), 3);
        assert_eq!(tokens[5].get_col(), 5);

        assert_eq!(tokens[6].get_name(), "SEMICOLON");
        assert_eq!(tokens[6].get_row(), 3);
        assert_eq!(tokens[6].get_col(), 6);
    }

    #[test]
    fn test_row_col_with_comments() {
        let language = create_test_language().unwrap();
        let lexer = Lexer::new(language).unwrap();
        let input = "// Line 1 comment\nvar x = /* multi-line\ncomment */ 10;";
        let tokens = lexer.lex(input).unwrap();

        assert_eq!(tokens[0].get_name(), "VAR_KEYWORD");
        assert_eq!(tokens[0].get_row(), 2);
        assert_eq!(tokens[0].get_col(), 1);

        assert_eq!(tokens[1].get_name(), "IDENTIFIER");
        assert_eq!(tokens[1].get_row(), 2);
        assert_eq!(tokens[1].get_col(), 5);

        assert_eq!(tokens[2].get_name(), "ASSIGN");
        assert_eq!(tokens[2].get_row(), 2);
        assert_eq!(tokens[2].get_col(), 7);

        assert_eq!(tokens[3].get_name(), "INTEGER_LITERAL");
        assert_eq!(tokens[3].get_row(), 3);
        assert_eq!(tokens[3].get_col(), 12);

        assert_eq!(tokens[4].get_name(), "SEMICOLON");
        assert_eq!(tokens[4].get_row(), 3);
        assert_eq!(tokens[4].get_col(), 14);
    }

    #[test]
    fn test_row_col_error_reporting() {
        let language = create_test_language().unwrap();
        let lexer = Lexer::new(language).unwrap();
        let input = "var x;\n$y = 10;";
        let result = lexer.lex(input);

        assert!(result.is_err());
        assert!(
            result
                .unwrap_err()
                .contains("Unexpected character at row 2 col 1: '$'")
        );

        let input_unterminated = "var z = /* unclosed";
        let result_unterminated = lexer.lex(input_unterminated);
        assert!(result_unterminated.is_err());
        assert!(result_unterminated.unwrap_err().contains(
            "Unterminated ignore block starting with token 'MULTI_LINE_COMMENT' at row: 1  col: 9"
        ));
    }

    #[test]
    fn test_row_col_crlf_newlines() {
        let language = create_test_language().unwrap();
        let lexer = Lexer::new(language).unwrap();
        let input = "line1;\r\nline2;";
        let tokens = lexer.lex(input).unwrap();

        assert_eq!(tokens[0].get_name(), "IDENTIFIER");
        assert_eq!(tokens[0].get_row(), 1);
        assert_eq!(tokens[0].get_col(), 1);

        assert_eq!(tokens[1].get_name(), "SEMICOLON");
        assert_eq!(tokens[1].get_row(), 1);
        assert_eq!(tokens[1].get_col(), 6);

        assert_eq!(tokens[2].get_name(), "IDENTIFIER");
        assert_eq!(tokens[2].get_row(), 2);
        assert_eq!(tokens[2].get_col(), 1);

        assert_eq!(tokens[3].get_name(), "SEMICOLON");
        assert_eq!(tokens[3].get_row(), 2);
        assert_eq!(tokens[3].get_col(), 6);
    }
}
