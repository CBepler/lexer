use std::collections::{HashMap, HashSet};

use crate::regex::{RangeType, Regex, RegexPattern, escapes::EscapeChar};

type StateId = usize;

#[derive(PartialEq, Eq, Hash, Clone)]
enum TransitionLabel {
    Char(char),
    Any,
    Escape(EscapeChar),
    Epsilon,
    StartAnchorAssertion,
    EndAnchorAssertion,
    CharSet(CharSetType),
}

#[derive(PartialEq, Eq, Hash, Clone)]
pub enum CharSetType {
    Positive(Vec<RangeType>),
    Negative(Vec<RangeType>),
}

#[derive(Clone)]
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

pub struct LexerNFA {
    start_state: StateId,
    transitions: HashMap<StateId, HashSet<(TransitionLabel, StateId)>>,
    accept_states: HashMap<StateId, String>,
    next_state_id: StateId,
}

impl LexerNFA {
    pub fn new(patterns: Vec<(String, Regex)>) -> Result<Self, String> {
        let mut lexer_nfa = LexerNFA {
            start_state: 0,
            transitions: HashMap::new(),
            accept_states: HashMap::new(),
            next_state_id: 1,
        };
        let global_start_state = lexer_nfa.start_state;
        for (token_type_name, regex) in patterns {
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
            //Needs priority added if multiples regexes can lead to same state (ex: if keyword or identifier)
            lexer_nfa
                .accept_states
                .insert(nfa_fragment.accept_state, token_type_name);
        }
        Ok(lexer_nfa)
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
            let min_nfa = self.parse_concatenation(&min_pattern)?;
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
            self.concatenate_nfas(vec![min_nfa, max_nfa])
        } else {
            return Err(format!(
                "Invalid Regex passed to parse_quantifier {:?}",
                pattern
            ));
        }
    }

    fn parse_optional(&mut self, pattern: &RegexPattern) -> Result<NFAFragment, String> {
        let mut frag = self.create_frag();
        let inner_nfa = self.compile_regex_pattern(pattern)?;
        frag.add_transition(
            frag.start_state,
            TransitionLabel::Epsilon,
            inner_nfa.start_state,
        );
        frag.add_transition(
            frag.start_state,
            TransitionLabel::Epsilon,
            inner_nfa.accept_state,
        );
        frag.add_transition(
            inner_nfa.accept_state,
            TransitionLabel::Epsilon,
            frag.accept_state,
        );
        Ok(frag)
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
