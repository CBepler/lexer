use std::collections::{BTreeSet, HashMap, HashSet, VecDeque};

use crate::{
    nfa::{self, LexerNFA},
    regex::escapes::EscapeChar,
};

type StateId = usize;

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
enum TransitionLabel {
    Char(char),
    Any,
    Escape(EscapeChar),
    StartAnchorAssertion,
    EndAnchorAssertion,
    CharSet(nfa::CharSetType),
}

impl TransitionLabel {
    fn from_nfa_transition(
        nfa_transition: &nfa::TransitionLabel,
    ) -> Result<TransitionLabel, String> {
        match nfa_transition {
            nfa::TransitionLabel::Any => Ok(TransitionLabel::Any),
            nfa::TransitionLabel::Char(x) => Ok(TransitionLabel::Char(*x)),
            nfa::TransitionLabel::CharSet(x) => Ok(TransitionLabel::CharSet(x.clone())),
            nfa::TransitionLabel::EndAnchorAssertion => Ok(TransitionLabel::EndAnchorAssertion),
            nfa::TransitionLabel::StartAnchorAssertion => Ok(TransitionLabel::StartAnchorAssertion),
            nfa::TransitionLabel::Escape(x) => Ok(TransitionLabel::Escape(*x)),
            nfa::TransitionLabel::Epsilon => Err("No Epsilons allowed in DFA".to_string()),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct LexerDFA {
    start_state: StateId,
    transitions: HashMap<StateId, HashMap<TransitionLabel, StateId>>,
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
        let mut dfa_transitions: HashMap<StateId, HashMap<TransitionLabel, StateId>> =
            HashMap::new();
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
            for transition_label in get_all_possible_non_epsilon_transitions(&nfa) {
                let mut next_nfa_states_after_move: HashSet<StateId> = HashSet::new();
                for &nfa_state_in_set in &current_nfa_states_set {
                    if let Some(nfa_transitions) = nfa.get_transitions().get(&nfa_state_in_set) {
                        for (nfa_trans_label, nfa_target_state) in nfa_transitions {
                            if nfa_trans_label == &transition_label {
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
                current_dfa_state_transitions.insert(
                    TransitionLabel::from_nfa_transition(&transition_label)?,
                    next_dfa_state_id,
                );
            }
            if !current_dfa_state_transitions.is_empty() {
                dfa_transitions.insert(current_dfa_state_id, current_dfa_state_transitions);
            }
        }

        Ok(LexerDFA {
            start_state: dfa_start_state_id,
            transitions: dfa_transitions,
            accept_states: dfa_accept_states,
            next_state_id: *self.next_state_id,
        })
    }
}

fn get_all_possible_non_epsilon_transitions(nfa: &LexerNFA) -> HashSet<nfa::TransitionLabel> {
    let mut labels = HashSet::new();
    for (_, transitions) in nfa.get_transitions() {
        for (label, _) in transitions {
            if !matches!(label, nfa::TransitionLabel::Epsilon) {
                labels.insert(label.clone());
            }
        }
    }
    labels
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
    use super::{TransitionLabel::*, *};
    use crate::regex::Regex;

    fn add_transition(
        transitions: &mut HashMap<StateId, HashMap<TransitionLabel, StateId>>,
        from: StateId,
        label: TransitionLabel,
        to: StateId,
    ) {
        transitions
            .entry(from)
            .or_insert_with(HashMap::new)
            .insert(label, to);
    }

    #[test]
    fn lexer_dfa_alternation() {
        let text = r"a|b";
        let regex = Regex::new(text).unwrap();
        let nfa = LexerNFA::new(vec![("ALTERNATION_TOKEN".to_string(), 1, regex)]).unwrap();
        let dfa = LexerDFA::new(nfa).unwrap();
        let mut expected_transitions = HashMap::new();
        let mut expected_accepts = HashMap::new();
        add_transition(&mut expected_transitions, 0, Char('a'), 1);
        add_transition(&mut expected_transitions, 0, Char('b'), 1);
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
        add_transition(&mut expected_transitions, 0, Char('a'), 1);
        add_transition(&mut expected_transitions, 1, Char('a'), 1);
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
        add_transition(&mut expected_transitions, 0, Char('a'), 1);
        add_transition(&mut expected_transitions, 0, Char('b'), 1);
        expected_accepts.insert(1, "ALTERNATION_TOKEN".to_string());
        let expected_dfa = LexerDFA {
            start_state: 0,
            transitions: expected_transitions,
            accept_states: expected_accepts,
            next_state_id: 2,
        };
        assert_eq!(dfa, expected_dfa);
    }
}
