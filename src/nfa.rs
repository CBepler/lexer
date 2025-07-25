use std::collections::{HashMap, HashSet};

use crate::regex::{RangeType, Regex, RegexPattern, escapes::EscapeChar};

type StateId = usize;

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub enum TransitionLabel {
    Char(char),
    Any,
    Escape(EscapeChar),
    Epsilon,
    StartAnchorAssertion,
    EndAnchorAssertion,
    CharSet(CharSetType),
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub enum CharSetType {
    Positive(Vec<RangeType>),
    Negative(Vec<RangeType>),
}

#[derive(Clone, Debug, PartialEq)]
struct NFAFragment {
    start_state: StateId,
    accept_state: StateId,
    transitions: HashMap<StateId, HashSet<(TransitionLabel, StateId)>>,
}

impl NFAFragment {
    fn new(start_state: StateId, accept_state: StateId) -> Self {
        NFAFragment {
            start_state,
            accept_state,
            transitions: HashMap::new(),
        }
    }

    fn add_transition(&mut self, from: StateId, label: TransitionLabel, to: StateId) -> bool {
        self.transitions
            .entry(from)
            .or_insert_with(HashSet::new)
            .insert((label, to))
    }
}

#[derive(Debug, PartialEq)]
pub struct LexerNFA {
    start_state: StateId,
    transitions: HashMap<StateId, HashSet<(TransitionLabel, StateId)>>,
    accept_states: HashMap<StateId, Vec<(String, i32)>>,
    next_state_id: StateId,
}

impl LexerNFA {
    pub fn new(patterns: Vec<(String, i32, Regex)>) -> Result<Self, String> {
        let mut lexer_nfa = LexerNFA {
            start_state: 0,
            transitions: HashMap::new(),
            accept_states: HashMap::new(),
            next_state_id: 1,
        };
        let global_start_state = lexer_nfa.start_state;
        for (token_type_name, priority, regex) in patterns {
            let mut fragment_compiler = NFAFragmentCompiler {
                next_state_id: &mut lexer_nfa.next_state_id,
            };
            let nfa_fragment = fragment_compiler.compile_regex_pattern(regex.get_pattern())?;
            lexer_nfa.add_transition(
                global_start_state,
                TransitionLabel::Epsilon,
                nfa_fragment.start_state,
            );
            for (from_state, trans_list) in nfa_fragment.transitions {
                lexer_nfa
                    .transitions
                    .entry(from_state)
                    .or_insert_with(HashSet::new)
                    .extend(trans_list);
            }

            let accept_vec = lexer_nfa
                .accept_states
                .entry(nfa_fragment.accept_state)
                .or_insert_with(Vec::new);
            accept_vec.push((token_type_name, priority));
            accept_vec.sort_by(|a, b| b.1.cmp(&a.1));
        }
        Ok(lexer_nfa)
    }

    pub fn get_transitions(&self) -> &HashMap<StateId, HashSet<(TransitionLabel, StateId)>> {
        &self.transitions
    }

    pub fn get_start_state(&self) -> &StateId {
        &self.start_state
    }

    pub fn get_accept_states(&self) -> &HashMap<StateId, Vec<(String, i32)>> {
        &self.accept_states
    }

    fn add_transition(&mut self, from: StateId, label: TransitionLabel, to: StateId) -> bool {
        self.transitions
            .entry(from)
            .or_insert_with(HashSet::new)
            .insert((label, to))
    }
}

struct NFAFragmentCompiler<'a> {
    next_state_id: &'a mut StateId,
}

impl<'a> NFAFragmentCompiler<'a> {
    fn new_state(&mut self) -> StateId {
        let id = *self.next_state_id;
        *self.next_state_id += 1;
        id
    }

    fn compile_regex_pattern(&mut self, pattern: &RegexPattern) -> Result<NFAFragment, String> {
        match pattern {
            RegexPattern::Literal(_) => self.parse_literal(pattern),
            RegexPattern::Quantifier(_, _, _) => self.parse_quantifier(pattern),
            RegexPattern::NotRange(_) => self.parse_not_range(pattern),
            RegexPattern::Group(_) => self.parse_group(pattern),
            RegexPattern::Range(_) => self.parse_range(pattern),
            RegexPattern::Concatenation(_) => self.parse_concatenation(pattern),
            RegexPattern::Alternation(_) => self.parse_alternation(pattern),
            RegexPattern::AnyCharacter => self.parse_any_character(pattern),
            RegexPattern::EscapeChar(_) => self.parse_escape_char(pattern),
            RegexPattern::StartAnchor => self.parse_start_anchor(pattern),
            RegexPattern::EndAnchor => self.parse_end_anchor(pattern),
        }
    }

    fn create_frag(&mut self) -> NFAFragment {
        let start_state = self.new_state();
        let accept_state = self.new_state();
        NFAFragment::new(start_state, accept_state)
    }

    fn parse_literal(&mut self, pattern: &RegexPattern) -> Result<NFAFragment, String> {
        let mut frag = self.create_frag();
        if let RegexPattern::Literal(char) = pattern {
            frag.add_transition(
                frag.start_state,
                TransitionLabel::Char(*char),
                frag.accept_state,
            );
        } else {
            return Err(format!(
                "Invalid Regex passed to parse_literal {:?}",
                pattern
            ));
        };
        Ok(frag)
    }

    fn parse_any_character(&mut self, pattern: &RegexPattern) -> Result<NFAFragment, String> {
        let mut frag = self.create_frag();
        if RegexPattern::AnyCharacter == *pattern {
            frag.add_transition(frag.start_state, TransitionLabel::Any, frag.accept_state);
        } else {
            return Err(format!(
                "Invalid Regex passed to parse_any_character {:?}",
                pattern
            ));
        };
        Ok(frag)
    }

    fn parse_escape_char(&mut self, pattern: &RegexPattern) -> Result<NFAFragment, String> {
        let mut frag = self.create_frag();
        if let RegexPattern::EscapeChar(char) = pattern {
            frag.add_transition(
                frag.start_state,
                TransitionLabel::Escape(*char),
                frag.accept_state,
            );
        } else {
            return Err(format!(
                "Invalid Regex passed to parse_escape_char {:?}",
                pattern
            ));
        };
        Ok(frag)
    }

    fn parse_range(&mut self, pattern: &RegexPattern) -> Result<NFAFragment, String> {
        let mut frag = self.create_frag();
        if let RegexPattern::Range(ranges) = pattern {
            frag.add_transition(
                frag.start_state,
                TransitionLabel::CharSet(CharSetType::Positive(ranges.to_vec())),
                frag.accept_state,
            );
        } else {
            return Err(format!("Invalid Regex passed to parse_range {:?}", pattern));
        };
        Ok(frag)
    }

    fn parse_not_range(&mut self, pattern: &RegexPattern) -> Result<NFAFragment, String> {
        let mut frag = self.create_frag();
        if let RegexPattern::NotRange(ranges) = pattern {
            frag.add_transition(
                frag.start_state,
                TransitionLabel::CharSet(CharSetType::Negative(ranges.to_vec())),
                frag.accept_state,
            );
        } else {
            return Err(format!(
                "Invalid Regex passed to parse_not_range {:?}",
                pattern
            ));
        };
        Ok(frag)
    }

    fn parse_concatenation(&mut self, pattern: &RegexPattern) -> Result<NFAFragment, String> {
        if let RegexPattern::Concatenation(parts) = pattern {
            let nfas_res: Result<Vec<NFAFragment>, String> = parts
                .into_iter()
                .map(|part| self.compile_regex_pattern(part))
                .collect();
            let nfas = nfas_res?;
            return Ok(self.concatenate_nfas(nfas)?);
        } else {
            return Err(format!(
                "Invalid Regex passed to parse_concatenation {:?}",
                pattern
            ));
        };
    }

    fn concatenate_nfas(&mut self, frags: Vec<NFAFragment>) -> Result<NFAFragment, String> {
        if frags.is_empty() {
            return Err("Empty Concatenation not Allowed".to_string());
        }
        let mut frag_iter = frags.into_iter();
        let current_fragment = frag_iter.next().unwrap();
        let overall_start_state = current_fragment.start_state;
        let mut overall_accept_state = current_fragment.accept_state;
        let mut combined_transitions = current_fragment.transitions;
        for next_fragment in frag_iter {
            combined_transitions
                .entry(overall_accept_state)
                .or_insert_with(HashSet::new)
                .insert((TransitionLabel::Epsilon, next_fragment.start_state));
            for (from_state, trans_list) in next_fragment.transitions {
                combined_transitions
                    .entry(from_state)
                    .or_insert_with(HashSet::new)
                    .extend(trans_list);
            }
            overall_accept_state = next_fragment.accept_state;
        }

        Ok(NFAFragment {
            start_state: overall_start_state,
            accept_state: overall_accept_state,
            transitions: combined_transitions,
        })
    }

    fn parse_alternation(&mut self, pattern: &RegexPattern) -> Result<NFAFragment, String> {
        let mut frag = self.create_frag();
        if let RegexPattern::Alternation(parts) = pattern {
            let nfas_res: Result<Vec<NFAFragment>, String> = parts
                .into_iter()
                .map(|part| self.compile_regex_pattern(part))
                .collect();
            let nfas = nfas_res?;
            for nfa in nfas {
                frag.add_transition(frag.start_state, TransitionLabel::Epsilon, nfa.start_state);
                frag.add_transition(
                    nfa.accept_state,
                    TransitionLabel::Epsilon,
                    frag.accept_state,
                );
                for (from_state, trans_list) in nfa.transitions {
                    frag.transitions
                        .entry(from_state)
                        .or_insert_with(HashSet::new)
                        .extend(trans_list);
                }
            }
        } else {
            return Err(format!(
                "Invalid Regex passed to parse_alternation {:?}",
                pattern
            ));
        };
        Ok(frag)
    }

    fn parse_quantifier(&mut self, pattern: &RegexPattern) -> Result<NFAFragment, String> {
        if let RegexPattern::Quantifier(pattern, min, max_op) = pattern {
            let min_pattern = RegexPattern::Concatenation(vec![pattern.clone(); *min]);
            let mut min_nfa = None;
            if *min > 0 {
                min_nfa = Some(self.parse_concatenation(&min_pattern)?);
            }
            let mut max_nfa;
            if let Some(max) = max_op {
                let diff = max - min;
                let max_nfas = vec![self.parse_optional(pattern)?; diff];
                max_nfa = self.concatenate_nfas(max_nfas)?;
            } else {
                max_nfa = self.compile_regex_pattern(pattern)?;
                max_nfa.add_transition(
                    max_nfa.start_state,
                    TransitionLabel::Epsilon,
                    max_nfa.accept_state,
                );
                max_nfa.add_transition(
                    max_nfa.accept_state,
                    TransitionLabel::Epsilon,
                    max_nfa.start_state,
                );
            }
            if let Some(min_nfa) = min_nfa {
                return self.concatenate_nfas(vec![min_nfa, max_nfa]);
            }
            Ok(max_nfa)
        } else {
            return Err(format!(
                "Invalid Regex passed to parse_quantifier {:?}",
                pattern
            ));
        }
    }

    fn parse_optional(&mut self, pattern: &RegexPattern) -> Result<NFAFragment, String> {
        let mut inner_nfa = self.compile_regex_pattern(pattern)?;
        inner_nfa.add_transition(
            inner_nfa.start_state,
            TransitionLabel::Epsilon,
            inner_nfa.accept_state,
        );
        Ok(inner_nfa)
    }

    fn parse_group(&mut self, pattern: &RegexPattern) -> Result<NFAFragment, String> {
        if let RegexPattern::Group(inner_pattern) = pattern {
            self.compile_regex_pattern(inner_pattern)
        } else {
            return Err(format!("Invalid Regex passed to parse_group {:?}", pattern));
        }
    }

    fn parse_start_anchor(&mut self, pattern: &RegexPattern) -> Result<NFAFragment, String> {
        let mut frag = self.create_frag();
        if RegexPattern::StartAnchor == *pattern {
            frag.add_transition(
                frag.start_state,
                TransitionLabel::StartAnchorAssertion,
                frag.accept_state,
            );
        } else {
            return Err(format!(
                "Invalid Regex passed to parse_start_anchor {:?}",
                pattern
            ));
        };
        Ok(frag)
    }

    fn parse_end_anchor(&mut self, pattern: &RegexPattern) -> Result<NFAFragment, String> {
        let mut frag = self.create_frag();
        if RegexPattern::EndAnchor == *pattern {
            frag.add_transition(
                frag.start_state,
                TransitionLabel::EndAnchorAssertion,
                frag.accept_state,
            );
        } else {
            return Err(format!(
                "Invalid Regex passed to parse_end_anchor {:?}",
                pattern
            ));
        };
        Ok(frag)
    }
}

#[cfg(test)]
mod tests {
    use std::vec;

    use super::*;

    fn add_transition(
        transitions: &mut HashMap<StateId, HashSet<(TransitionLabel, StateId)>>,
        from: StateId,
        label: TransitionLabel,
        to: StateId,
    ) {
        transitions
            .entry(from)
            .or_insert_with(HashSet::new)
            .insert((label, to));
    }

    #[test]
    fn lexor_nfa_single_simple_concat() {
        let text = r"ab";
        let regex = Regex::new(text).unwrap();

        let nfa = LexerNFA::new(vec![("AB_TOKEN".to_string(), 1, regex)]).unwrap();

        let mut expected_transitions: HashMap<StateId, HashSet<(TransitionLabel, StateId)>> =
            HashMap::new();
        let mut expected_accept_states: HashMap<StateId, Vec<(String, i32)>> = HashMap::new();

        add_transition(&mut expected_transitions, 0, TransitionLabel::Epsilon, 1);
        add_transition(&mut expected_transitions, 1, TransitionLabel::Char('a'), 2);
        add_transition(&mut expected_transitions, 2, TransitionLabel::Epsilon, 3);
        add_transition(&mut expected_transitions, 3, TransitionLabel::Char('b'), 4);

        expected_accept_states.insert(4, vec![("AB_TOKEN".to_string(), 1)]);

        let expected_nfa = LexerNFA {
            start_state: 0,
            transitions: expected_transitions,
            accept_states: expected_accept_states,
            next_state_id: 5,
        };

        assert_eq!(nfa, expected_nfa);
    }

    #[test]
    fn lexor_nfa_single_literal() {
        let text = r"x";
        let regex = Regex::new(text).unwrap();

        let nfa = LexerNFA::new(vec![("X_TOKEN".to_string(), 1, regex)]).unwrap();

        let mut expected_transitions: HashMap<StateId, HashSet<(TransitionLabel, StateId)>> =
            HashMap::new();
        let mut expected_accept_states: HashMap<StateId, Vec<(String, i32)>> = HashMap::new();

        add_transition(&mut expected_transitions, 0, TransitionLabel::Epsilon, 1);
        add_transition(&mut expected_transitions, 1, TransitionLabel::Char('x'), 2);

        expected_accept_states.insert(2, vec![("X_TOKEN".to_string(), 1)]);

        let expected_nfa = LexerNFA {
            start_state: 0,
            transitions: expected_transitions,
            accept_states: expected_accept_states,
            next_state_id: 3,
        };

        assert_eq!(nfa, expected_nfa);
    }

    #[test]
    fn lexor_nfa_any() {
        let text = r".";
        let regex = Regex::new(text).unwrap();

        let nfa = LexerNFA::new(vec![("ANY_TOKEN".to_string(), 1, regex)]).unwrap();

        let mut expected_transitions: HashMap<StateId, HashSet<(TransitionLabel, StateId)>> =
            HashMap::new();
        let mut expected_accept_states: HashMap<StateId, Vec<(String, i32)>> = HashMap::new();

        add_transition(&mut expected_transitions, 0, TransitionLabel::Epsilon, 1);
        add_transition(&mut expected_transitions, 1, TransitionLabel::Any, 2);

        expected_accept_states.insert(2, vec![("ANY_TOKEN".to_string(), 1)]);

        let expected_nfa = LexerNFA {
            start_state: 0,
            transitions: expected_transitions,
            accept_states: expected_accept_states,
            next_state_id: 3,
        };

        assert_eq!(nfa, expected_nfa);
    }

    #[test]
    fn lexor_nfa_alternation() {
        let text = r"a|b";
        let regex = Regex::new(text).unwrap();

        let nfa = LexerNFA::new(vec![("OR_TOKEN".to_string(), 1, regex)]).unwrap();

        let mut expected_transitions: HashMap<StateId, HashSet<(TransitionLabel, StateId)>> =
            HashMap::new();
        let mut expected_accept_states: HashMap<StateId, Vec<(String, i32)>> = HashMap::new();

        add_transition(&mut expected_transitions, 0, TransitionLabel::Epsilon, 1);
        add_transition(&mut expected_transitions, 1, TransitionLabel::Epsilon, 5);
        add_transition(&mut expected_transitions, 1, TransitionLabel::Epsilon, 3);
        add_transition(&mut expected_transitions, 3, TransitionLabel::Char('a'), 4);
        add_transition(&mut expected_transitions, 5, TransitionLabel::Char('b'), 6);
        add_transition(&mut expected_transitions, 4, TransitionLabel::Epsilon, 2);
        add_transition(&mut expected_transitions, 6, TransitionLabel::Epsilon, 2);

        expected_accept_states.insert(2, vec![("OR_TOKEN".to_string(), 1)]);

        let expected_nfa = LexerNFA {
            start_state: 0,
            transitions: expected_transitions,
            accept_states: expected_accept_states,
            next_state_id: 7,
        };

        assert_eq!(nfa, expected_nfa);
    }

    #[test]
    fn lexor_nfa_range() {
        let text = r"[a-c]";
        let regex = Regex::new(text).unwrap();

        let nfa = LexerNFA::new(vec![("RANGE_TOKEN".to_string(), 1, regex)]).unwrap();

        let mut expected_transitions: HashMap<StateId, HashSet<(TransitionLabel, StateId)>> =
            HashMap::new();
        let mut expected_accept_states: HashMap<StateId, Vec<(String, i32)>> = HashMap::new();

        add_transition(&mut expected_transitions, 0, TransitionLabel::Epsilon, 1);
        let ranges = vec![RangeType::MultiChar('a', 'c')];
        add_transition(
            &mut expected_transitions,
            1,
            TransitionLabel::CharSet(CharSetType::Positive(ranges)),
            2,
        );

        expected_accept_states.insert(2, vec![("RANGE_TOKEN".to_string(), 1)]);

        let expected_nfa = LexerNFA {
            start_state: 0,
            transitions: expected_transitions,
            accept_states: expected_accept_states,
            next_state_id: 3,
        };

        assert_eq!(nfa, expected_nfa);
    }

    #[test]
    fn lexor_nfa_not_range() {
        let text = r"[^a-c]";
        let regex = Regex::new(text).unwrap();

        let nfa = LexerNFA::new(vec![("NOT_RANGE_TOKEN".to_string(), 1, regex)]).unwrap();

        let mut expected_transitions: HashMap<StateId, HashSet<(TransitionLabel, StateId)>> =
            HashMap::new();
        let mut expected_accept_states: HashMap<StateId, Vec<(String, i32)>> = HashMap::new();

        add_transition(&mut expected_transitions, 0, TransitionLabel::Epsilon, 1);
        let ranges = vec![RangeType::MultiChar('a', 'c')];
        add_transition(
            &mut expected_transitions,
            1,
            TransitionLabel::CharSet(CharSetType::Negative(ranges)),
            2,
        );

        expected_accept_states.insert(2, vec![("NOT_RANGE_TOKEN".to_string(), 1)]);

        let expected_nfa = LexerNFA {
            start_state: 0,
            transitions: expected_transitions,
            accept_states: expected_accept_states,
            next_state_id: 3,
        };

        assert_eq!(nfa, expected_nfa);
    }

    #[test]
    fn lexor_nfa_kleene_star() {
        let text = r"a*";
        let regex = Regex::new(text).unwrap();

        let nfa = LexerNFA::new(vec![("STAR_TOKEN".to_string(), 1, regex)]).unwrap();

        let mut expected_transitions: HashMap<StateId, HashSet<(TransitionLabel, StateId)>> =
            HashMap::new();
        let mut expected_accept_states: HashMap<StateId, Vec<(String, i32)>> = HashMap::new();

        add_transition(&mut expected_transitions, 0, TransitionLabel::Epsilon, 1);
        add_transition(&mut expected_transitions, 1, TransitionLabel::Char('a'), 2);
        add_transition(&mut expected_transitions, 1, TransitionLabel::Epsilon, 2);
        add_transition(&mut expected_transitions, 2, TransitionLabel::Epsilon, 1);

        expected_accept_states.insert(2, vec![("STAR_TOKEN".to_string(), 1)]);

        let expected_nfa = LexerNFA {
            start_state: 0,
            transitions: expected_transitions,
            accept_states: expected_accept_states,
            next_state_id: 3,
        };

        assert_eq!(nfa, expected_nfa);
    }

    #[test]
    fn lexor_nfa_kleene_plus() {
        let text = r"a+";
        let regex = Regex::new(text).unwrap();

        let nfa = LexerNFA::new(vec![("PLUS_TOKEN".to_string(), 1, regex)]).unwrap();

        let mut expected_transitions: HashMap<StateId, HashSet<(TransitionLabel, StateId)>> =
            HashMap::new();
        let mut expected_accept_states: HashMap<StateId, Vec<(String, i32)>> = HashMap::new();

        add_transition(&mut expected_transitions, 0, TransitionLabel::Epsilon, 1);
        add_transition(&mut expected_transitions, 1, TransitionLabel::Char('a'), 2);
        add_transition(&mut expected_transitions, 2, TransitionLabel::Epsilon, 3);
        add_transition(&mut expected_transitions, 3, TransitionLabel::Epsilon, 4);
        add_transition(&mut expected_transitions, 3, TransitionLabel::Epsilon, 4);
        add_transition(&mut expected_transitions, 3, TransitionLabel::Char('a'), 4);
        add_transition(&mut expected_transitions, 4, TransitionLabel::Epsilon, 3);

        expected_accept_states.insert(4, vec![("PLUS_TOKEN".to_string(), 1)]);

        let expected_nfa = LexerNFA {
            start_state: 0,
            transitions: expected_transitions,
            accept_states: expected_accept_states,
            next_state_id: 5,
        };

        assert_eq!(nfa, expected_nfa);
    }

    #[test]
    fn lexor_nfa_optional() {
        let text = r"a?";
        let regex = Regex::new(text).unwrap();

        let nfa = LexerNFA::new(vec![("OPTIONAL_TOKEN".to_string(), 1, regex)]).unwrap();

        let mut expected_transitions: HashMap<StateId, HashSet<(TransitionLabel, StateId)>> =
            HashMap::new();
        let mut expected_accept_states: HashMap<StateId, Vec<(String, i32)>> = HashMap::new();

        add_transition(&mut expected_transitions, 0, TransitionLabel::Epsilon, 1);
        add_transition(&mut expected_transitions, 1, TransitionLabel::Char('a'), 2);
        add_transition(&mut expected_transitions, 1, TransitionLabel::Epsilon, 2);

        expected_accept_states.insert(2, vec![("OPTIONAL_TOKEN".to_string(), 1)]);

        let expected_nfa = LexerNFA {
            start_state: 0,
            transitions: expected_transitions,
            accept_states: expected_accept_states,
            next_state_id: 3,
        };

        assert_eq!(nfa, expected_nfa);
    }

    #[test]
    fn lexor_nfa_group() {
        let text = r"(a)";
        let regex = Regex::new(text).unwrap();

        let nfa = LexerNFA::new(vec![("GROUP_TOKEN".to_string(), 1, regex)]).unwrap();

        let mut expected_transitions: HashMap<StateId, HashSet<(TransitionLabel, StateId)>> =
            HashMap::new();
        let mut expected_accept_states: HashMap<StateId, Vec<(String, i32)>> = HashMap::new();

        add_transition(&mut expected_transitions, 0, TransitionLabel::Epsilon, 1);
        add_transition(&mut expected_transitions, 1, TransitionLabel::Char('a'), 2);

        expected_accept_states.insert(2, vec![("GROUP_TOKEN".to_string(), 1)]);

        let expected_nfa = LexerNFA {
            start_state: 0,
            transitions: expected_transitions,
            accept_states: expected_accept_states,
            next_state_id: 3,
        };

        assert_eq!(nfa, expected_nfa);
    }

    #[test]
    fn lexor_nfa_multiple_patterns() {
        let regex_a = Regex::new(r"a").unwrap();
        let regex_b = Regex::new(r"b").unwrap();
        let nfa = LexerNFA::new(vec![
            ("TOKEN_A".to_string(), 1, regex_a),
            ("TOKEN_B".to_string(), 2, regex_b),
        ])
        .unwrap();

        let mut expected_transitions: HashMap<StateId, HashSet<(TransitionLabel, StateId)>> =
            HashMap::new();
        let mut expected_accept_states: HashMap<StateId, Vec<(String, i32)>> = HashMap::new();

        add_transition(&mut expected_transitions, 0, TransitionLabel::Epsilon, 1);
        add_transition(&mut expected_transitions, 1, TransitionLabel::Char('a'), 2);
        expected_accept_states.insert(2, vec![("TOKEN_A".to_string(), 1)]);

        add_transition(&mut expected_transitions, 0, TransitionLabel::Epsilon, 3);
        add_transition(&mut expected_transitions, 3, TransitionLabel::Char('b'), 4);
        expected_accept_states.insert(4, vec![("TOKEN_B".to_string(), 2)]);

        let expected_nfa = LexerNFA {
            start_state: 0,
            transitions: expected_transitions,
            accept_states: expected_accept_states,
            next_state_id: 5,
        };

        assert_eq!(nfa, expected_nfa,);
    }

    #[test]
    fn lexor_nfa_start_anchor() {
        let text = r"^a";
        let regex = Regex::new(text).unwrap();

        let nfa = LexerNFA::new(vec![("START_ANCHOR_TOKEN".to_string(), 1, regex)]).unwrap();

        let mut expected_transitions: HashMap<StateId, HashSet<(TransitionLabel, StateId)>> =
            HashMap::new();
        let mut expected_accept_states: HashMap<StateId, Vec<(String, i32)>> = HashMap::new();

        add_transition(&mut expected_transitions, 0, TransitionLabel::Epsilon, 1);
        add_transition(
            &mut expected_transitions,
            1,
            TransitionLabel::StartAnchorAssertion,
            2,
        );
        add_transition(&mut expected_transitions, 2, TransitionLabel::Epsilon, 3);
        add_transition(&mut expected_transitions, 3, TransitionLabel::Char('a'), 4);

        expected_accept_states.insert(4, vec![("START_ANCHOR_TOKEN".to_string(), 1)]);

        let expected_nfa = LexerNFA {
            start_state: 0,
            transitions: expected_transitions,
            accept_states: expected_accept_states,
            next_state_id: 5,
        };

        assert_eq!(nfa, expected_nfa);
    }

    #[test]
    fn lexor_nfa_end_anchor() {
        let text = r"a$";
        let regex = Regex::new(text).unwrap();

        let nfa = LexerNFA::new(vec![("END_ANCHOR_TOKEN".to_string(), 1, regex)]).unwrap();

        let mut expected_transitions: HashMap<StateId, HashSet<(TransitionLabel, StateId)>> =
            HashMap::new();
        let mut expected_accept_states: HashMap<StateId, Vec<(String, i32)>> = HashMap::new();

        add_transition(&mut expected_transitions, 0, TransitionLabel::Epsilon, 1);
        add_transition(&mut expected_transitions, 1, TransitionLabel::Char('a'), 2);
        add_transition(&mut expected_transitions, 2, TransitionLabel::Epsilon, 3);
        add_transition(
            &mut expected_transitions,
            3,
            TransitionLabel::EndAnchorAssertion,
            4,
        );

        expected_accept_states.insert(4, vec![("END_ANCHOR_TOKEN".to_string(), 1)]);

        let expected_nfa = LexerNFA {
            start_state: 0,
            transitions: expected_transitions,
            accept_states: expected_accept_states,
            next_state_id: 5,
        };

        assert_eq!(nfa, expected_nfa);
    }

    #[test]
    fn lexor_nfa_full_line_match() {
        let text = r"^a$";
        let regex = Regex::new(text).unwrap();
        let nfa = LexerNFA::new(vec![("FULL_LINE_TOKEN".to_string(), 1, regex)]).unwrap();

        let mut expected_transitions: HashMap<StateId, HashSet<(TransitionLabel, StateId)>> =
            HashMap::new();
        let mut expected_accept_states: HashMap<StateId, Vec<(String, i32)>> = HashMap::new();

        add_transition(&mut expected_transitions, 0, TransitionLabel::Epsilon, 1);
        add_transition(
            &mut expected_transitions,
            1,
            TransitionLabel::StartAnchorAssertion,
            2,
        );
        add_transition(&mut expected_transitions, 2, TransitionLabel::Epsilon, 3);
        add_transition(&mut expected_transitions, 3, TransitionLabel::Char('a'), 4);
        add_transition(&mut expected_transitions, 4, TransitionLabel::Epsilon, 5);
        add_transition(
            &mut expected_transitions,
            5,
            TransitionLabel::EndAnchorAssertion,
            6,
        );

        expected_accept_states.insert(6, vec![("FULL_LINE_TOKEN".to_string(), 1)]);

        let expected_nfa = LexerNFA {
            start_state: 0,
            transitions: expected_transitions,
            accept_states: expected_accept_states,
            next_state_id: 7,
        };

        assert_eq!(nfa, expected_nfa);
    }

    #[test]
    fn lexor_nfa_escape_digit() {
        let text = r"\d";
        let regex = Regex::new(text).unwrap();
        let nfa = LexerNFA::new(vec![("DIGIT_TOKEN".to_string(), 1, regex)]).unwrap();

        let mut expected_transitions: HashMap<StateId, HashSet<(TransitionLabel, StateId)>> =
            HashMap::new();
        let mut expected_accept_states: HashMap<StateId, Vec<(String, i32)>> = HashMap::new();

        add_transition(&mut expected_transitions, 0, TransitionLabel::Epsilon, 1);
        add_transition(
            &mut expected_transitions,
            1,
            TransitionLabel::Escape(EscapeChar::Digit),
            2,
        );

        expected_accept_states.insert(2, vec![("DIGIT_TOKEN".to_string(), 1)]);

        let expected_nfa = LexerNFA {
            start_state: 0,
            transitions: expected_transitions,
            accept_states: expected_accept_states,
            next_state_id: 3,
        };

        assert_eq!(nfa, expected_nfa);
    }

    #[test]
    fn lexor_nfa_general_quantifier() {
        let text = r"a{1,2}";
        let regex = Regex::new(text).unwrap();
        let nfa = LexerNFA::new(vec![("QUANTIFIER_TOKEN".to_string(), 1, regex)]).unwrap();

        let mut expected_transitions: HashMap<StateId, HashSet<(TransitionLabel, StateId)>> =
            HashMap::new();
        let mut expected_accept_states: HashMap<StateId, Vec<(String, i32)>> = HashMap::new();

        add_transition(&mut expected_transitions, 0, TransitionLabel::Epsilon, 1);

        add_transition(&mut expected_transitions, 1, TransitionLabel::Char('a'), 2);

        add_transition(&mut expected_transitions, 2, TransitionLabel::Epsilon, 3);

        add_transition(&mut expected_transitions, 3, TransitionLabel::Epsilon, 4);
        add_transition(&mut expected_transitions, 3, TransitionLabel::Char('a'), 4);

        expected_accept_states.insert(4, vec![("QUANTIFIER_TOKEN".to_string(), 1)]);

        let expected_nfa = LexerNFA {
            start_state: 0,
            transitions: expected_transitions,
            accept_states: expected_accept_states,
            next_state_id: 5,
        };

        assert_eq!(nfa, expected_nfa);
    }

    #[test]
    fn lexer_nfa_multiple_overlapping_accept() {
        let text1 = r"^if$";
        let text2 = r"^[a-zA-Z_][a-zA-Z0-9_]*$";
        let regex1 = Regex::new(text1).unwrap();
        let regex2 = Regex::new(text2).unwrap();
        let nfa = LexerNFA::new(vec![
            ("IF_TOKEN".to_string(), 2, regex1),
            ("IDENTIFIER_TOKEN".to_string(), 1, regex2),
        ])
        .unwrap();

        let mut expected_transitions: HashMap<StateId, HashSet<(TransitionLabel, StateId)>> =
            HashMap::new();
        let mut expected_accept_states: HashMap<StateId, Vec<(String, i32)>> = HashMap::new();

        let start_ranges = vec![
            RangeType::MultiChar('a', 'z'),
            RangeType::MultiChar('A', 'Z'),
            RangeType::SingleChar('_'),
        ];

        let end_ranges = vec![
            RangeType::MultiChar('a', 'z'),
            RangeType::MultiChar('A', 'Z'),
            RangeType::MultiChar('0', '9'),
            RangeType::SingleChar('_'),
        ];

        add_transition(&mut expected_transitions, 0, TransitionLabel::Epsilon, 1);
        add_transition(&mut expected_transitions, 0, TransitionLabel::Epsilon, 9);

        add_transition(
            &mut expected_transitions,
            1,
            TransitionLabel::StartAnchorAssertion,
            2,
        );

        add_transition(&mut expected_transitions, 2, TransitionLabel::Epsilon, 3);

        add_transition(&mut expected_transitions, 3, TransitionLabel::Char('i'), 4);

        add_transition(&mut expected_transitions, 4, TransitionLabel::Epsilon, 5);

        add_transition(&mut expected_transitions, 5, TransitionLabel::Char('f'), 6);

        add_transition(&mut expected_transitions, 6, TransitionLabel::Epsilon, 7);

        add_transition(
            &mut expected_transitions,
            7,
            TransitionLabel::EndAnchorAssertion,
            8,
        );

        add_transition(
            &mut expected_transitions,
            9,
            TransitionLabel::StartAnchorAssertion,
            10,
        );

        add_transition(&mut expected_transitions, 10, TransitionLabel::Epsilon, 11);

        add_transition(
            &mut expected_transitions,
            11,
            TransitionLabel::CharSet(CharSetType::Positive(start_ranges)),
            12,
        );

        add_transition(&mut expected_transitions, 12, TransitionLabel::Epsilon, 13);

        add_transition(&mut expected_transitions, 13, TransitionLabel::Epsilon, 14);
        add_transition(
            &mut expected_transitions,
            13,
            TransitionLabel::CharSet(CharSetType::Positive(end_ranges)),
            14,
        );

        add_transition(&mut expected_transitions, 14, TransitionLabel::Epsilon, 13);

        add_transition(&mut expected_transitions, 14, TransitionLabel::Epsilon, 15);

        add_transition(
            &mut expected_transitions,
            15,
            TransitionLabel::EndAnchorAssertion,
            16,
        );

        expected_accept_states.insert(8, vec![("IF_TOKEN".to_string(), 2)]);
        expected_accept_states.insert(16, vec![("IDENTIFIER_TOKEN".to_string(), 1)]);

        let expected_nfa = LexerNFA {
            start_state: 0,
            transitions: expected_transitions,
            accept_states: expected_accept_states,
            next_state_id: 17,
        };

        assert_eq!(nfa, expected_nfa);
    }

    #[test]
    fn lexor_nfa_complex_pattern() {
        let text = r"^(\d|[a-f])+\s?$";
        let regex = Regex::new(text).unwrap();
        let nfa = LexerNFA::new(vec![("COMPLEX_TOKEN".to_string(), 1, regex)]).unwrap();

        let mut expected_transitions: HashMap<StateId, HashSet<(TransitionLabel, StateId)>> =
            HashMap::new();
        let mut expected_accept_states: HashMap<StateId, Vec<(String, i32)>> = HashMap::new();

        let hex_ranges = vec![RangeType::MultiChar('a', 'f')];

        add_transition(&mut expected_transitions, 0, TransitionLabel::Epsilon, 1);
        add_transition(
            &mut expected_transitions,
            1,
            TransitionLabel::StartAnchorAssertion,
            2,
        );
        add_transition(&mut expected_transitions, 2, TransitionLabel::Epsilon, 3);

        add_transition(&mut expected_transitions, 3, TransitionLabel::Epsilon, 5);
        add_transition(&mut expected_transitions, 3, TransitionLabel::Epsilon, 7);

        add_transition(
            &mut expected_transitions,
            5,
            TransitionLabel::Escape(EscapeChar::Digit),
            6,
        );
        add_transition(
            &mut expected_transitions,
            7,
            TransitionLabel::CharSet(CharSetType::Positive(hex_ranges.clone())),
            8,
        );

        add_transition(&mut expected_transitions, 6, TransitionLabel::Epsilon, 4);
        add_transition(&mut expected_transitions, 8, TransitionLabel::Epsilon, 4);

        add_transition(&mut expected_transitions, 4, TransitionLabel::Epsilon, 9);

        add_transition(&mut expected_transitions, 9, TransitionLabel::Epsilon, 11);
        add_transition(&mut expected_transitions, 9, TransitionLabel::Epsilon, 13);
        add_transition(&mut expected_transitions, 9, TransitionLabel::Epsilon, 10);

        add_transition(&mut expected_transitions, 10, TransitionLabel::Epsilon, 9);
        add_transition(&mut expected_transitions, 10, TransitionLabel::Epsilon, 15);

        add_transition(
            &mut expected_transitions,
            11,
            TransitionLabel::Escape(EscapeChar::Digit),
            12,
        );
        add_transition(&mut expected_transitions, 12, TransitionLabel::Epsilon, 10);

        add_transition(
            &mut expected_transitions,
            13,
            TransitionLabel::CharSet(CharSetType::Positive(hex_ranges)),
            14,
        );
        add_transition(&mut expected_transitions, 14, TransitionLabel::Epsilon, 10);

        add_transition(&mut expected_transitions, 15, TransitionLabel::Epsilon, 16);
        add_transition(
            &mut expected_transitions,
            15,
            TransitionLabel::Escape(EscapeChar::Whitespace),
            16,
        );

        add_transition(&mut expected_transitions, 16, TransitionLabel::Epsilon, 17);

        add_transition(
            &mut expected_transitions,
            17,
            TransitionLabel::EndAnchorAssertion,
            18,
        );

        expected_accept_states.insert(18, vec![("COMPLEX_TOKEN".to_string(), 1)]);

        let expected_nfa = LexerNFA {
            start_state: 0,
            transitions: expected_transitions,
            accept_states: expected_accept_states,
            next_state_id: 19,
        };

        assert_eq!(nfa, expected_nfa);
    }
}
