use std::collections::{BTreeSet, HashMap, HashSet, VecDeque};

use crate::{
    nfa::{self, LexerNFA},
    regex::escapes::EscapeChar,
};

type StateId = usize;

#[derive(Debug, PartialEq)]
pub struct Transitions {
    start_anchor_assertion_target: Option<StateId>,
    end_anchor_assertion_target: Option<StateId>,
    word_boundry_assertion_target: Option<StateId>,
    range_transitions: Vec<((char, char), StateId)>,
}

impl Transitions {
    fn new(mut transition_map: HashMap<TransitionLabel, StateId>) -> Result<Self, String> {
        let start_anchor_assertion_target =
            transition_map.remove(&TransitionLabel::StartAnchorAssertion);
        let end_anchor_assertion_target =
            transition_map.remove(&TransitionLabel::EndAnchorAssertion);
        let word_boundry_assertion_target = transition_map.remove(&TransitionLabel::WordBoundry);
        let mut range_transitions = Vec::new();
        for (key, val) in transition_map {
            match key {
                TransitionLabel::Range(low, high) => range_transitions.push(((low, high), val)),
                x => return Err(format!("Unexpected transition in transition_map: {:?}", x)),
            }
        }
        range_transitions.sort_by_key(|range| range.0.0);
        Ok(Transitions {
            start_anchor_assertion_target,
            end_anchor_assertion_target,
            word_boundry_assertion_target,
            range_transitions,
        })
    }
}

#[derive(PartialEq, Eq, Hash, Clone, Debug, Ord, PartialOrd)]
pub enum TransitionLabel {
    StartAnchorAssertion,
    EndAnchorAssertion,
    WordBoundry,
    Range(char, char),
}

impl TransitionLabel {
    fn from_nfa_transition(
        nfa_transition: &nfa::TransitionLabel,
    ) -> Result<Vec<TransitionLabel>, String> {
        match nfa_transition {
            nfa::TransitionLabel::Any => Ok(vec![TransitionLabel::Range('\0', '\u{10FFFF}')]),
            nfa::TransitionLabel::Char(x) => Ok(vec![TransitionLabel::Range(*x, *x)]),
            nfa::TransitionLabel::CharSet(x) => Ok(x
                .to_char_range()
                .iter()
                .map(|(low, high)| TransitionLabel::Range(*low, *high))
                .collect()),
            nfa::TransitionLabel::EndAnchorAssertion => {
                Ok(vec![TransitionLabel::EndAnchorAssertion])
            }
            nfa::TransitionLabel::StartAnchorAssertion => {
                Ok(vec![TransitionLabel::StartAnchorAssertion])
            }
            nfa::TransitionLabel::Escape(x) => Ok(match x {
                EscapeChar::WordBoundry => vec![TransitionLabel::WordBoundry],
                x => x
                    .matching_ascii()
                    .iter()
                    .map(|(low, high)| TransitionLabel::Range(*low, *high))
                    .collect(),
            }),
            nfa::TransitionLabel::Epsilon => Err("No Epsilons allowed in DFA".to_string()),
        }
    }

    fn encompasses(&self, other: &Self) -> bool {
        match self {
            TransitionLabel::StartAnchorAssertion => {
                other == &TransitionLabel::StartAnchorAssertion
            }
            TransitionLabel::EndAnchorAssertion => other == &TransitionLabel::EndAnchorAssertion,
            TransitionLabel::WordBoundry => other == &TransitionLabel::WordBoundry,
            TransitionLabel::Range(low, high) => {
                if let TransitionLabel::Range(other_low, other_high) = other {
                    return (low <= other_low) & (high >= other_high);
                } else {
                    return false;
                }
            }
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct LexerDFA {
    start_state: StateId,
    transitions: HashMap<StateId, Transitions>,
    accept_states: HashMap<StateId, String>,
    next_state_id: StateId,
}

impl LexerDFA {
    pub fn new(nfa: LexerNFA) -> Result<Self, String> {
        let mut next_state_id_counter: StateId = 0;
        let mut constructor = DFAConstructor {
            next_state_id: &mut next_state_id_counter,
        };
        constructor.construct(nfa)
    }

    pub fn get_start_state(&self) -> StateId {
        self.start_state
    }

    pub fn get_state_transitions(&self, current_state: StateId) -> Option<&Transitions> {
        self.transitions.get(&current_state)
    }
}

struct DFAConstructor<'a> {
    next_state_id: &'a mut StateId,
}

impl<'a> DFAConstructor<'a> {
    fn new_state(&mut self) -> StateId {
        let id = *self.next_state_id;
        *self.next_state_id += 1;
        id
    }

    fn construct(&mut self, nfa: LexerNFA) -> Result<LexerDFA, String> {
        let state_closures = get_state_closures(&nfa);
        let mut nfa_sets_to_dfa_ids: HashMap<BTreeSet<StateId>, StateId> = HashMap::new();
        let mut unmarked_dfa_states_queue: VecDeque<StateId> = VecDeque::new();
        let mut dfa_transitions: HashMap<StateId, Transitions> = HashMap::new();
        let mut dfa_accept_states: HashMap<StateId, String> = HashMap::new();
        let initial_nfa_closure: BTreeSet<StateId> = state_closures
            .get(nfa.get_start_state())
            .expect("NFA start state must have a closure")
            .clone()
            .into_iter()
            .collect();
        let dfa_start_state_id = self.new_state();
        nfa_sets_to_dfa_ids.insert(initial_nfa_closure.clone(), dfa_start_state_id);
        unmarked_dfa_states_queue.push_back(dfa_start_state_id);
        while let Some(current_dfa_state_id) = unmarked_dfa_states_queue.pop_front() {
            let current_nfa_states_set = nfa_sets_to_dfa_ids
                .iter()
                .find_map(|(k, &v)| {
                    if v == current_dfa_state_id {
                        Some(k)
                    } else {
                        None
                    }
                })
                .expect("DFA state ID not found in nfa_sets_to_dfa_ids")
                .clone();
            println!("Current NFA State set: {:?}", current_nfa_states_set);
            let mut possible_accepts: Vec<(String, i32)> = Vec::new();
            for &nfa_state in &current_nfa_states_set {
                if let Some(nfa_accept_info) = nfa.get_accept_states().get(&nfa_state) {
                    possible_accepts.extend(nfa_accept_info.iter().cloned());
                }
            }
            if !possible_accepts.is_empty() {
                possible_accepts.sort_by(|a, b| b.1.cmp(&a.1));
                dfa_accept_states.insert(current_dfa_state_id, possible_accepts.remove(0).0);
            }
            let mut current_dfa_state_transitions: HashMap<TransitionLabel, StateId> =
                HashMap::new();
            for transition_label in get_all_possible_dfa_transitions(&nfa, &current_nfa_states_set)
            {
                let mut next_nfa_states_after_move: HashSet<StateId> = HashSet::new();
                for &nfa_state_in_set in &current_nfa_states_set {
                    if let Some(nfa_transitions) = nfa.get_transitions().get(&nfa_state_in_set) {
                        for (nfa_trans_label, nfa_target_state) in nfa_transitions {
                            if *nfa_trans_label == nfa::TransitionLabel::Epsilon {
                                continue;
                            }
                            println!("NFA Trans label: {:?}", nfa_trans_label);
                            let dfa_transitions =
                                TransitionLabel::from_nfa_transition(nfa_trans_label).unwrap();
                            println!("DFA transition: {:?}", dfa_transitions);
                            if dfa_transitions
                                .iter()
                                .any(|transition| transition.encompasses(&transition_label))
                            {
                                next_nfa_states_after_move.insert(*nfa_target_state);
                            }
                        }
                    }
                }
                if next_nfa_states_after_move.is_empty() {
                    continue;
                }

                let mut next_dfa_nfa_set: BTreeSet<StateId> = BTreeSet::new();
                for &state_id_in_move_result in &next_nfa_states_after_move {
                    if let Some(closure) = state_closures.get(&state_id_in_move_result) {
                        next_dfa_nfa_set.extend(closure.iter().cloned());
                    } else {
                        next_dfa_nfa_set.insert(state_id_in_move_result);
                    }
                }
                let next_dfa_state_id =
                    if let Some(&existing_dfa_id) = nfa_sets_to_dfa_ids.get(&next_dfa_nfa_set) {
                        existing_dfa_id
                    } else {
                        let new_id = self.new_state();
                        nfa_sets_to_dfa_ids.insert(next_dfa_nfa_set.clone(), new_id);
                        unmarked_dfa_states_queue.push_back(new_id);
                        new_id
                    };
                current_dfa_state_transitions.insert(transition_label, next_dfa_state_id);
            }
            let compiled_dfa_state_transitions = Transitions::new(current_dfa_state_transitions)?;
            dfa_transitions.insert(current_dfa_state_id, compiled_dfa_state_transitions);
        }

        Ok(LexerDFA {
            start_state: dfa_start_state_id,
            transitions: dfa_transitions,
            accept_states: dfa_accept_states,
            next_state_id: *self.next_state_id,
        })
    }
}

fn get_all_possible_dfa_transitions(
    nfa: &LexerNFA,
    current_nfa_state_set: &BTreeSet<StateId>,
) -> BTreeSet<TransitionLabel> {
    let mut transitions = Vec::new();
    let nfa_transitions = nfa.get_transitions();
    let mut has_start_anchor = false;
    let mut has_end_anchor = false;
    let mut has_word_boundry = false;
    println!("Getting possible transitions");
    for state in current_nfa_state_set {
        if let Some(state_transitions) = nfa_transitions.get(&state) {
            for (transition, _) in state_transitions {
                let dfa_transition_res = TransitionLabel::from_nfa_transition(transition);
                let dfa_transitions = match dfa_transition_res {
                    Ok(transitions) => transitions,
                    Err(_) => continue,
                };
                for dfa_transition in dfa_transitions {
                    match dfa_transition {
                        TransitionLabel::StartAnchorAssertion => has_start_anchor = true,
                        TransitionLabel::EndAnchorAssertion => has_end_anchor = true,
                        TransitionLabel::WordBoundry => has_word_boundry = true,
                        TransitionLabel::Range(low, high) => {
                            transitions.push((low, high));
                        }
                    }
                }
            }
        }
    }
    println!("Transitions raw: {:?}", transitions);
    transitions.sort_by_key(|range| (*range).0);
    let mut disjoint_transitions = BTreeSet::new();
    let mut range_iter = transitions.into_iter();
    if let Some((mut start, mut end)) = range_iter.next() {
        while let Some((next_start, next_end)) = range_iter.next() {
            if end >= next_start {
                if end > next_end {
                    if (next_start as u32) - (start as u32) > 1 {
                        disjoint_transitions.insert(TransitionLabel::Range(
                            start,
                            char::from_u32((next_start as u32) - 1).unwrap(),
                        ));
                    }
                    disjoint_transitions.insert(TransitionLabel::Range(next_start, next_end));
                    let start_op = char::from_u32((next_end as u32) + 1);
                    match start_op {
                        Some(s) => start = s,
                        None => break,
                    }
                } else if end == next_end {
                    disjoint_transitions.insert(TransitionLabel::Range(start, next_start));
                    let start_op = char::from_u32((next_start as u32) + 1);
                    match start_op {
                        Some(s) => start = s,
                        None => break,
                    }
                } else {
                    if (start as u32) - (next_start as u32) > 1 {
                        disjoint_transitions.insert(TransitionLabel::Range(
                            start,
                            char::from_u32((next_start as u32) - 1).unwrap(),
                        ));
                    }
                    disjoint_transitions.insert(TransitionLabel::Range(next_start, end));
                    start = char::from_u32((end as u32) + 1).unwrap();
                    end = next_end;
                }
            } else {
                disjoint_transitions.insert(TransitionLabel::Range(start, end));
                start = next_start;
                end = next_end;
            }
        }
        disjoint_transitions.insert(TransitionLabel::Range(start, end));
    }
    if has_start_anchor {
        disjoint_transitions.insert(TransitionLabel::StartAnchorAssertion);
    }
    if has_end_anchor {
        disjoint_transitions.insert(TransitionLabel::EndAnchorAssertion);
    }
    if has_word_boundry {
        disjoint_transitions.insert(TransitionLabel::WordBoundry);
    }
    println!("Reduce transitions: {:?}", disjoint_transitions);
    disjoint_transitions
}

fn get_state_closures(nfa: &LexerNFA) -> HashMap<StateId, HashSet<StateId>> {
    let mut epsilon_closure = HashMap::new();
    for (state, transitions) in nfa.get_transitions() {
        epsilon_closure.insert(*state, HashSet::new());
        for (transition_type, to_state) in transitions {
            if *transition_type == nfa::TransitionLabel::Epsilon {
                epsilon_closure.entry(*state).and_modify(|closure| {
                    closure.insert(*to_state);
                });
            }
        }
    }
    loop {
        let mut changed = false;
        let mut updates: HashMap<StateId, HashSet<StateId>> = HashMap::new();
        for (state, current_closure) in &epsilon_closure {
            let mut new_additions_for_state = HashSet::new();
            for reached_state_via_epsilon in current_closure {
                if let Some(transitive_closure_from_reached) =
                    epsilon_closure.get(reached_state_via_epsilon)
                {
                    new_additions_for_state.extend(transitive_closure_from_reached.iter().cloned());
                }
            }
            if !new_additions_for_state.is_empty() {
                updates.insert(*state, new_additions_for_state);
            }
        }
        for (state, additions) in updates {
            let closure_for_state = epsilon_closure.get_mut(&state).unwrap();
            let initial_len = closure_for_state.len();
            closure_for_state.extend(additions);
            if closure_for_state.len() > initial_len {
                changed = true;
            }
        }
        if !changed {
            break;
        }
    }
    epsilon_closure
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::regex::Regex;

    fn create_expected_transitions(
        start_anchor: Option<StateId>,
        end_anchor: Option<StateId>,
        word_boundary: Option<StateId>,
        mut ranges: Vec<((char, char), StateId)>,
    ) -> Transitions {
        ranges.sort_by_key(|range| range.0.0);
        Transitions {
            start_anchor_assertion_target: start_anchor,
            end_anchor_assertion_target: end_anchor,
            word_boundry_assertion_target: word_boundary,
            range_transitions: ranges,
        }
    }

    #[test]
    fn lexer_dfa_alternation() {
        let text = r"a|b";
        let regex = Regex::new(text).unwrap();
        let nfa = LexerNFA::new(vec![("ALTERNATION_TOKEN".to_string(), 1, regex)]).unwrap();
        let dfa = LexerDFA::new(nfa).unwrap(); // Add .unwrap() for Result

        let mut expected_transitions = HashMap::new();
        let mut expected_accepts = HashMap::new();

        expected_transitions.insert(
            0,
            create_expected_transitions(None, None, None, vec![(('a', 'a'), 1), (('b', 'b'), 1)]),
        );

        expected_transitions.insert(1, create_expected_transitions(None, None, None, vec![]));

        expected_accepts.insert(1, "ALTERNATION_TOKEN".to_string());

        let expected_dfa = LexerDFA {
            start_state: 0,
            transitions: expected_transitions,
            accept_states: expected_accepts,
            next_state_id: 2,
        };
        assert_eq!(dfa, expected_dfa);
    }

    #[test]
    fn lexer_dfa_kleene_plus() {
        let text = r"a+";
        let regex = Regex::new(text).unwrap();
        let nfa = LexerNFA::new(vec![("PLUS_TOKEN".to_string(), 1, regex)]).unwrap();
        let dfa = LexerDFA::new(nfa).unwrap();

        let mut expected_transitions = HashMap::new();
        let mut expected_accepts = HashMap::new();

        expected_transitions.insert(
            0,
            create_expected_transitions(None, None, None, vec![(('a', 'a'), 1)]),
        );

        expected_transitions.insert(
            1,
            create_expected_transitions(None, None, None, vec![(('a', 'a'), 1)]),
        );

        expected_accepts.insert(1, "PLUS_TOKEN".to_string());

        let expected_dfa = LexerDFA {
            start_state: 0,
            transitions: expected_transitions,
            accept_states: expected_accepts,
            next_state_id: 2,
        };
        assert_eq!(dfa, expected_dfa);
    }

    #[test]
    fn lexer_dfa_multiple_overlapping_accept() {
        let text1 = r"^if$";
        let text2 = r"^[a-zA-Z_][a-zA-Z0-9_]*$";
        let regex1 = Regex::new(text1).unwrap();
        let regex2 = Regex::new(text2).unwrap();
        let nfa = LexerNFA::new(vec![
            ("IF_TOKEN".to_string(), 2, regex1),
            ("IDENTIFIER_TOKEN".to_string(), 1, regex2),
        ])
        .unwrap();
        let dfa = LexerDFA::new(nfa).unwrap();

        let mut expected_transitions = HashMap::new();
        let mut expected_accepts = HashMap::new();

        expected_transitions.insert(0, create_expected_transitions(Some(1), None, None, vec![]));

        expected_transitions.insert(
            1,
            create_expected_transitions(
                None,
                None,
                None,
                vec![
                    (('A', 'Z'), 2),
                    (('a', 'h'), 2),
                    (('i', 'i'), 3),
                    (('j', 'z'), 2),
                    (('_', '_'), 2),
                ],
            ),
        );

        expected_transitions.insert(
            2,
            create_expected_transitions(
                None,
                Some(4),
                None,
                vec![
                    (('0', '9'), 2),
                    (('A', 'Z'), 2),
                    (('_', '_'), 2),
                    (('a', 'z'), 2),
                ],
            ),
        );

        expected_transitions.insert(
            3,
            create_expected_transitions(
                None,
                Some(4),
                None,
                vec![
                    (('0', '9'), 2),
                    (('A', 'Z'), 2),
                    (('a', 'e'), 2),
                    (('f', 'f'), 5),
                    (('g', 'z'), 2),
                    (('_', '_'), 2),
                ],
            ),
        );

        expected_transitions.insert(
            5,
            create_expected_transitions(
                None,
                Some(6),
                None,
                vec![
                    (('0', '9'), 2),
                    (('A', 'Z'), 2),
                    (('a', 'z'), 2),
                    (('_', '_'), 2),
                ],
            ),
        );

        expected_transitions.insert(4, create_expected_transitions(None, None, None, vec![]));

        expected_transitions.insert(6, create_expected_transitions(None, None, None, vec![]));

        expected_accepts.insert(4, "IDENTIFIER_TOKEN".to_string());
        expected_accepts.insert(6, "IF_TOKEN".to_string());

        let expected_dfa = LexerDFA {
            start_state: 0,
            transitions: expected_transitions,
            accept_states: expected_accepts,
            next_state_id: 7,
        };
        assert_eq!(dfa, expected_dfa);
    }

    #[test]
    fn lexer_dfa_multiple_overlapping_accept_no_anchor() {
        let text1 = r"if";
        let text2 = r"[a-zA-Z_][a-zA-Z0-9_]*";
        let regex1 = Regex::new(text1).unwrap();
        let regex2 = Regex::new(text2).unwrap();
        let nfa = LexerNFA::new(vec![
            ("IF_TOKEN".to_string(), 2, regex1),
            ("IDENTIFIER_TOKEN".to_string(), 1, regex2),
        ])
        .unwrap();
        let dfa = LexerDFA::new(nfa).unwrap();

        let mut expected_transitions = HashMap::new();
        let mut expected_accepts = HashMap::new();

        expected_transitions.insert(
            0,
            create_expected_transitions(
                None,
                None,
                None,
                vec![
                    (('A', 'Z'), 1),
                    (('a', 'h'), 1),
                    (('i', 'i'), 2),
                    (('j', 'z'), 1),
                    (('_', '_'), 1),
                ],
            ),
        );

        expected_transitions.insert(
            1,
            create_expected_transitions(
                None,
                None,
                None,
                vec![
                    (('0', '9'), 1),
                    (('A', 'Z'), 1),
                    (('_', '_'), 1),
                    (('a', 'z'), 1),
                ],
            ),
        );

        expected_transitions.insert(
            2,
            create_expected_transitions(
                None,
                None,
                None,
                vec![
                    (('0', '9'), 1),
                    (('A', 'Z'), 1),
                    (('a', 'e'), 1),
                    (('f', 'f'), 3),
                    (('g', 'z'), 1),
                    (('_', '_'), 1),
                ],
            ),
        );

        expected_transitions.insert(
            3,
            create_expected_transitions(
                None,
                None,
                None,
                vec![
                    (('0', '9'), 1),
                    (('A', 'Z'), 1),
                    (('_', '_'), 1),
                    (('a', 'z'), 1),
                ],
            ),
        );

        expected_accepts.insert(1, "IDENTIFIER_TOKEN".to_string());
        expected_accepts.insert(2, "IDENTIFIER_TOKEN".to_string());
        expected_accepts.insert(3, "IF_TOKEN".to_string());
        let expected_dfa = LexerDFA {
            start_state: 0,
            transitions: expected_transitions,
            accept_states: expected_accepts,
            next_state_id: 4,
        };
        assert_eq!(dfa, expected_dfa);
    }

    #[test]
    fn lexer_dfa_single_char_quantifier_optional() {
        let text = r"a?";
        let regex = Regex::new(text).unwrap();
        let nfa = LexerNFA::new(vec![("OPTIONAL_A".to_string(), 1, regex)]).unwrap();
        let dfa = LexerDFA::new(nfa).unwrap();

        let mut expected_transitions = HashMap::new();
        let mut expected_accepts = HashMap::new();

        expected_accepts.insert(0, "OPTIONAL_A".to_string());

        expected_transitions.insert(
            0,
            create_expected_transitions(None, None, None, vec![(('a', 'a'), 1)]),
        );

        expected_transitions.insert(1, create_expected_transitions(None, None, None, vec![]));
        expected_accepts.insert(1, "OPTIONAL_A".to_string());

        let expected_dfa = LexerDFA {
            start_state: 0,
            transitions: expected_transitions,
            accept_states: expected_accepts,
            next_state_id: 2,
        };
        assert_eq!(dfa, expected_dfa);
    }

    #[test]
    fn lexer_dfa_kleene_star() {
        let text = r"a*";
        let regex = Regex::new(text).unwrap();
        let nfa = LexerNFA::new(vec![("STAR_A".to_string(), 1, regex)]).unwrap();
        let dfa = LexerDFA::new(nfa).unwrap();

        let mut expected_transitions = HashMap::new();
        let mut expected_accepts = HashMap::new();

        expected_transitions.insert(
            0,
            create_expected_transitions(None, None, None, vec![(('a', 'a'), 0)]),
        );

        expected_accepts.insert(0, "STAR_A".to_string());

        let expected_dfa = LexerDFA {
            start_state: 0,
            transitions: expected_transitions,
            accept_states: expected_accepts,
            next_state_id: 1,
        };
        assert_eq!(dfa, expected_dfa);
    }

    #[test]
    fn lexer_dfa_digit_char_set() {
        let text = r"\d";
        let regex = Regex::new(text).unwrap();
        let nfa = LexerNFA::new(vec![("DIGIT_TOKEN".to_string(), 1, regex)]).unwrap();
        let dfa = LexerDFA::new(nfa).unwrap();

        let mut expected_transitions = HashMap::new();
        let mut expected_accepts = HashMap::new();

        expected_transitions.insert(
            0,
            create_expected_transitions(None, None, None, vec![(('0', '9'), 1)]),
        );

        expected_transitions.insert(1, create_expected_transitions(None, None, None, vec![]));
        expected_accepts.insert(1, "DIGIT_TOKEN".to_string());

        let expected_dfa = LexerDFA {
            start_state: 0,
            transitions: expected_transitions,
            accept_states: expected_accepts,
            next_state_id: 2,
        };
        assert_eq!(dfa, expected_dfa);
    }

    #[test]
    fn lexer_dfa_word_char_set_plus() {
        let text = r"\w+";
        let regex = Regex::new(text).unwrap();
        let nfa = LexerNFA::new(vec![("WORD_TOKEN".to_string(), 1, regex)]).unwrap();
        let dfa = LexerDFA::new(nfa).unwrap();

        let mut expected_transitions = HashMap::new();
        let mut expected_accepts = HashMap::new();

        expected_transitions.insert(
            0,
            create_expected_transitions(
                None,
                None,
                None,
                vec![
                    (('0', '9'), 1),
                    (('A', 'Z'), 1),
                    (('_', '_'), 1),
                    (('a', 'z'), 1),
                ],
            ),
        );

        expected_transitions.insert(
            1,
            create_expected_transitions(
                None,
                None,
                None,
                vec![
                    (('0', '9'), 1),
                    (('A', 'Z'), 1),
                    (('_', '_'), 1),
                    (('a', 'z'), 1),
                ],
            ),
        );

        expected_accepts.insert(1, "WORD_TOKEN".to_string());

        let expected_dfa = LexerDFA {
            start_state: 0,
            transitions: expected_transitions,
            accept_states: expected_accepts,
            next_state_id: 2,
        };
        assert_eq!(dfa, expected_dfa);
    }

    #[test]
    fn lexer_dfa_whitespace_char() {
        let text = r"\s";
        let regex = Regex::new(text).unwrap();
        let nfa = LexerNFA::new(vec![("WHITESPACE_TOKEN".to_string(), 1, regex)]).unwrap();
        let dfa = LexerDFA::new(nfa).unwrap();

        let mut expected_transitions = HashMap::new();
        let mut expected_accepts = HashMap::new();

        expected_transitions.insert(
            0,
            create_expected_transitions(
                None,
                None,
                None,
                vec![
                    ((' ', ' '), 1),
                    (('\n', '\n'), 1),
                    (('\r', '\r'), 1),
                    (('\t', '\t'), 1),
                    (('\u{000C}', '\u{000C}'), 1),
                ],
            ),
        );

        expected_transitions.insert(1, create_expected_transitions(None, None, None, vec![]));

        expected_accepts.insert(1, "WHITESPACE_TOKEN".to_string());

        let expected_dfa = LexerDFA {
            start_state: 0,
            transitions: expected_transitions,
            accept_states: expected_accepts,
            next_state_id: 2,
        };
        assert_eq!(dfa, expected_dfa);
    }

    #[test]
    fn lexer_dfa_ranges_and_concatenation() {
        let text = r"c[a-c]t";
        let regex = Regex::new(text).unwrap();
        let nfa = LexerNFA::new(vec![("COMPLEX_TOKEN".to_string(), 1, regex)]).unwrap();
        let dfa = LexerDFA::new(nfa).unwrap();

        let mut expected_transitions = HashMap::new();
        let mut expected_accepts = HashMap::new();

        expected_transitions.insert(
            0,
            create_expected_transitions(None, None, None, vec![(('c', 'c'), 1)]),
        );

        expected_transitions.insert(
            1,
            create_expected_transitions(None, None, None, vec![(('a', 'c'), 2)]),
        );

        expected_transitions.insert(
            2,
            create_expected_transitions(None, None, None, vec![(('t', 't'), 3)]),
        );

        expected_transitions.insert(3, create_expected_transitions(None, None, None, vec![]));
        expected_accepts.insert(3, "COMPLEX_TOKEN".to_string());

        let expected_dfa = LexerDFA {
            start_state: 0,
            transitions: expected_transitions,
            accept_states: expected_accepts,
            next_state_id: 4,
        };
        assert_eq!(dfa, expected_dfa);
    }

    #[test]
    fn lexer_dfa_dot_any_char() {
        let text = r".";
        let regex = Regex::new(text).unwrap();
        let nfa = LexerNFA::new(vec![("ANY_CHAR_TOKEN".to_string(), 1, regex)]).unwrap();
        let dfa = LexerDFA::new(nfa).unwrap();

        let mut expected_transitions = HashMap::new();
        let mut expected_accepts = HashMap::new();

        expected_transitions.insert(
            0,
            create_expected_transitions(None, None, None, vec![(('\0', '\u{10FFFF}'), 1)]),
        );

        expected_transitions.insert(1, create_expected_transitions(None, None, None, vec![]));

        expected_accepts.insert(1, "ANY_CHAR_TOKEN".to_string());

        let expected_dfa = LexerDFA {
            start_state: 0,
            transitions: expected_transitions,
            accept_states: expected_accepts,
            next_state_id: 2,
        };
        assert_eq!(dfa, expected_dfa);
    }

    #[test]
    fn lexer_dfa_word_boundary_at_start() {
        let text = r"\bword";
        let regex = Regex::new(text).unwrap();
        let nfa = LexerNFA::new(vec![("BW_WORD".to_string(), 1, regex)]).unwrap();
        let dfa = LexerDFA::new(nfa).unwrap();

        let mut expected_transitions = HashMap::new();
        let mut expected_accepts = HashMap::new();

        expected_transitions.insert(0, create_expected_transitions(None, None, Some(1), vec![]));

        expected_transitions.insert(
            1,
            create_expected_transitions(None, None, None, vec![(('w', 'w'), 2)]),
        );

        expected_transitions.insert(
            2,
            create_expected_transitions(None, None, None, vec![(('o', 'o'), 3)]),
        );

        expected_transitions.insert(
            3,
            create_expected_transitions(None, None, None, vec![(('r', 'r'), 4)]),
        );

        expected_transitions.insert(
            4,
            create_expected_transitions(None, None, None, vec![(('d', 'd'), 5)]),
        );

        expected_transitions.insert(5, create_expected_transitions(None, None, None, vec![]));
        expected_accepts.insert(5, "BW_WORD".to_string());

        let expected_dfa = LexerDFA {
            start_state: 0,
            transitions: expected_transitions,
            accept_states: expected_accepts,
            next_state_id: 6,
        };
        assert_eq!(dfa, expected_dfa);
    }

    #[test]
    fn lexer_dfa_word_boundary_at_end() {
        let text = r"word\b";
        let regex = Regex::new(text).unwrap();
        let nfa = LexerNFA::new(vec![("WORD_BW".to_string(), 1, regex)]).unwrap();
        let dfa = LexerDFA::new(nfa).unwrap();

        let mut expected_transitions = HashMap::new();
        let mut expected_accepts = HashMap::new();

        expected_transitions.insert(
            0,
            create_expected_transitions(None, None, None, vec![(('w', 'w'), 1)]),
        );

        expected_transitions.insert(
            1,
            create_expected_transitions(None, None, None, vec![(('o', 'o'), 2)]),
        );

        expected_transitions.insert(
            2,
            create_expected_transitions(None, None, None, vec![(('r', 'r'), 3)]),
        );

        expected_transitions.insert(
            3,
            create_expected_transitions(None, None, None, vec![(('d', 'd'), 4)]),
        );

        expected_transitions.insert(4, create_expected_transitions(None, None, Some(5), vec![]));

        expected_transitions.insert(5, create_expected_transitions(None, None, None, vec![]));
        expected_accepts.insert(5, "WORD_BW".to_string());

        let expected_dfa = LexerDFA {
            start_state: 0,
            transitions: expected_transitions,
            accept_states: expected_accepts,
            next_state_id: 6,
        };
        assert_eq!(dfa, expected_dfa);
    }
}
