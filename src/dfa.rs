use std::collections::{HashMap, HashSet};

use crate::{
    nfa::{self, LexerNFA},
    regex::{RangeType, escapes::EscapeChar},
};

type StateId = usize;

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
enum TransitionLabel {
    Char(char),
    Any,
    Escape(EscapeChar),
    StartAnchorAssertion,
    EndAnchorAssertion,
    CharSet(CharSetType),
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub enum CharSetType {
    Positive(Vec<RangeType>),
    Negative(Vec<RangeType>),
}

struct lexer_dfa {
    start_state: StateId,
    transitions: HashMap<StateId, HashSet<(TransitionLabel, StateId)>>,
    accept_states: HashMap<StateId, Vec<(String, i32)>>,
    next_state_id: StateId,
}

impl lexer_dfa {
    pub fn new(nfa: LexerNFA) -> Self {
        let state_closures = get_state_closures(&nfa);
    }
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
