use std::collections::{HashMap, HashSet, VecDeque};

use crate::parser::grammar::{AmbiguousToken, Grammar, Symbol};

type NodeId = usize;

#[derive(Hash, PartialEq, Eq, Clone, Debug)]
struct ProductionRule {
    head: String,
    body: Vec<Symbol>,
    pos: usize,
}

impl ProductionRule {
    fn new(head: String, body: Vec<Symbol>, pos: usize) -> Self {
        ProductionRule { head, body, pos }
    }
}

#[derive(Debug)]
struct State {
    id: NodeId,
    //terminal or non-terminal to next state
    transitions: HashMap<Symbol, NodeId>,
    //terminal to reductions with (non-terminal name, body length)
    reductions: HashSet<(String, usize)>,
    production_rules: Vec<ProductionRule>,
    is_accept_state: bool,
}

struct StateConstructor {
    next_id: usize,
}

impl StateConstructor {
    fn new() -> Self {
        StateConstructor { next_id: 0 }
    }

    fn new_state(&mut self) -> State {
        let id = self.next_id;
        self.next_id += 1;
        State {
            id,
            transitions: HashMap::new(),
            reductions: HashSet::new(),
            production_rules: Vec::new(),
            is_accept_state: false,
        }
    }
}

struct GssNode {
    state: NodeId,
    parents: Vec<GssNode>,
}

struct Glr {
    grammar: Grammar,
    //Map from non-terminals to terminals that can start them
    firsts: HashMap<String, HashSet<String>>,
    //Map from non-terminals to terminals that can follow
    follows: HashMap<String, HashSet<String>>,
    states: HashMap<NodeId, State>,
    gss_root: GssNode,
}

impl Glr {
    pub fn new(grammar: Grammar) -> Result<Self, String> {
        let (firsts, follows) = construct_firsts_follows(&grammar);
        let (states, start_id) = construct_states(&grammar);
        let gss_root = GssNode {
            state: start_id,
            parents: Vec::new(),
        };
        Ok(Glr {
            grammar,
            firsts,
            follows,
            states,
            gss_root,
        })
    }
}

fn construct_firsts_follows(
    grammar: &Grammar,
) -> (
    HashMap<String, HashSet<String>>,
    HashMap<String, HashSet<String>>,
) {
    let mut firsts: HashMap<String, HashSet<String>> = HashMap::new();
    let mut follows: HashMap<String, HashSet<String>> = HashMap::new();
    let mut changed = true;
    for non_terminal in &mut grammar.production_rules.keys() {
        firsts.insert(non_terminal.clone(), HashSet::new());
        follows.insert(non_terminal.clone(), HashSet::new());
    }
    while changed {
        changed = false;
        for (head, bodies) in &grammar.production_rules {
            let mut body_iter = bodies.iter();
            while let Some(body) = body_iter.next() {
                let mut found_firsts = Vec::new();
                let mut symbol_iter = body.iter().peekable();
                while let Some(symbol) = symbol_iter.next() {
                    match symbol {
                        Symbol::Terminal(x) => {
                            found_firsts.push(x.to_string());
                            break;
                        }
                        Symbol::NonTerminal(x) => {
                            for first in firsts.get(x).unwrap() {
                                found_firsts.push(first.to_string())
                            }
                            if !grammar.derives_empty.contains(x) {
                                break;
                            }
                        }
                    }
                }
                let first_set = firsts.get_mut(head).unwrap();
                for first in found_firsts {
                    if first_set.insert(first) {
                        changed = true;
                    }
                }
                let mut symbol_iter = body.iter().peekable();
                let mut active = Vec::new();
                while let Some(symbol) = symbol_iter.next() {
                    match symbol {
                        Symbol::Terminal(x) => {
                            for a in &active {
                                let follow_set = follows.get_mut(*a).unwrap();
                                if follow_set.insert(x.to_string()) {
                                    changed = true;
                                }
                            }
                            active.clear();
                        }
                        Symbol::NonTerminal(x) => {
                            for a in &active {
                                let follow_set = follows.get_mut(*a).unwrap();
                                for first in firsts.get(x).unwrap() {
                                    if follow_set.insert(first.to_string()) {
                                        changed = true;
                                    }
                                }
                            }
                            if !grammar.derives_empty.contains(x) {
                                active.clear();
                            }
                            active.push(x);
                        }
                    }
                }
            }
        }
    }
    (firsts, follows)
}

fn construct_states(grammar: &Grammar) -> (HashMap<NodeId, State>, NodeId) {
    let mut production_map: HashMap<Vec<ProductionRule>, State> = HashMap::new();
    let mut state_constructor = StateConstructor::new();
    let mut start_state = state_constructor.new_state();
    let start_id = start_state.id;
    start_state.production_rules = get_closure(
        "S'",
        &vec![Symbol::NonTerminal(grammar.start_symbol.clone())],
        0,
        grammar,
    );
    start_state.is_accept_state = true;

    let mut states_to_process = VecDeque::new();
    states_to_process.push_back(start_state);

    while let Some(mut state) = states_to_process.pop_front() {
        let rules: Vec<(&String, &Vec<Symbol>, usize)> = state
            .production_rules
            .iter()
            .map(|rule| (&rule.head, &rule.body, rule.pos))
            .collect();
        let mut transitions: HashMap<Symbol, NodeId> = HashMap::new();
        let mut reductions: HashSet<(String, usize)> = HashSet::new();
        let mut possible_transitions: HashMap<Symbol, Vec<ProductionRule>> = HashMap::new();
        for (head, body, pos) in rules {
            let next_symbol = body.get(pos);
            match next_symbol {
                Some(symbol) => {
                    possible_transitions
                        .entry(symbol.clone())
                        .or_insert_with(Vec::new);
                    let transitions = possible_transitions.get_mut(symbol).unwrap();
                    transitions.extend(get_closure(head, body, pos + 1, grammar));
                }
                None => {
                    reductions.insert((head.to_string(), pos));
                }
            }
        }
        for (symbol, rules) in possible_transitions {
            if production_map.contains_key(&rules) {
                transitions.insert(symbol, production_map.get(&rules).unwrap().id);
            } else {
                let mut new_state = state_constructor.new_state();
                new_state.production_rules = rules;
                transitions.insert(symbol, new_state.id);
                states_to_process.push_back(new_state);
            }
        }
        state.transitions = transitions;
        state.reductions = reductions;
        production_map.insert(state.production_rules.clone(), state);
    }

    let state_map = production_map
        .into_values()
        .map(|state| (state.id, state))
        .collect();
    (state_map, start_id)
}

fn get_closure(
    head: &str,
    body: &Vec<Symbol>,
    pos: usize,
    grammar: &Grammar,
) -> Vec<ProductionRule> {
    let mut productions = Vec::new();
    let mut to_process = VecDeque::new();
    let mut excludes = HashSet::new();
    to_process.push_back(ProductionRule::new(head.to_string(), body.clone(), pos));
    while let Some(next) = to_process.pop_front() {
        let next_symbol = next.body.get(next.pos);
        if let Some(symbol) = next_symbol {
            match symbol {
                Symbol::NonTerminal(x) => {
                    if !excludes.contains(x) {
                        let rules = grammar.production_rules.get(x).unwrap();
                        for body in rules {
                            to_process.push_back(ProductionRule::new(
                                x.to_string(),
                                body.clone(),
                                0,
                            ));
                        }
                    }
                    excludes.insert(x.to_string());
                }
                Symbol::Terminal(_) => (),
            }
        }
        productions.push(next);
    }
    productions
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::grammar;

    #[test]
    fn test_glr_creation_simple_grammar() {
        let start_symbol = String::from("S");
        let production_rules = vec![
            grammar::ProductionRule::new(
                "S".to_string(),
                vec![
                    Symbol::NonTerminal("A".to_string()),
                    Symbol::NonTerminal("B".to_string()),
                ],
            ),
            grammar::ProductionRule::new("A".to_string(), vec![Symbol::Terminal("a".to_string())]),
            grammar::ProductionRule::new("B".to_string(), vec![Symbol::Terminal("b".to_string())]),
        ];
        let grammar = Grammar::new(
            start_symbol,
            vec!["a".to_string(), "b".to_string()],
            production_rules,
            Vec::new(),
        )
        .unwrap();

        let glr_parser = Glr::new(grammar).unwrap();

        let firsts = glr_parser.firsts;
        assert!(firsts.get("A").is_some());
        if let Some(firsts) = firsts.get("A") {
            assert!(firsts.contains("a"));
        }
        assert!(firsts.get("B").is_some());
        if let Some(firsts) = firsts.get("B") {
            assert!(firsts.contains("b"));
        }

        assert!(firsts.get("S").is_some());
        if let Some(firsts) = firsts.get("S") {
            assert!(firsts.contains("a"));
        }

        let follows = glr_parser.follows;

        assert!(follows.get("A").is_some());
        if let Some(follows) = follows.get("A") {
            assert!(follows.contains("b"));
        }

        let states = glr_parser.states;

        let start_state = states.values().find(|s| s.is_accept_state).unwrap();
        // S' -> .S, S -> .AB, A -> .a
        assert!(
            start_state
                .transitions
                .contains_key(&Symbol::NonTerminal("S".to_string()))
        );
        assert!(
            start_state
                .transitions
                .contains_key(&Symbol::NonTerminal("A".to_string()))
        );
        assert!(
            start_state
                .transitions
                .contains_key(&Symbol::Terminal("a".to_string()))
        );

        let state_after_a_id = *start_state
            .transitions
            .get(&Symbol::Terminal("a".to_string()))
            .unwrap();
        let state_after_a = states.get(&state_after_a_id).unwrap();
        // A -> a.
        assert!(
            state_after_a
                .reductions
                .contains(&("A".to_string(), 1 as usize))
        );

        let state_after_big_a_id = *start_state
            .transitions
            .get(&Symbol::NonTerminal("A".to_string()))
            .unwrap();
        let state_after_big_a = states.get(&state_after_big_a_id).unwrap();
        // S -> A.B, B -> .b
        assert!(
            state_after_big_a
                .transitions
                .contains_key(&Symbol::Terminal("b".to_string()))
        );

        let state_after_b_id = *state_after_big_a
            .transitions
            .get(&Symbol::Terminal("b".to_string()))
            .unwrap();
        let state_after_b = states.get(&state_after_b_id).unwrap();
        assert_eq!(state_after_b.production_rules.len(), 1); // B -> b.
        assert!(
            state_after_b
                .reductions
                .contains(&("B".to_string(), 1 as usize))
        );

        let state_after_big_s_id = *start_state
            .transitions
            .get(&Symbol::NonTerminal("S".to_string()))
            .unwrap();
        let state_after_big_s = states.get(&state_after_big_s_id).unwrap();
        // S' -> S.
        assert!(
            state_after_big_s
                .reductions
                .contains(&("S'".to_string(), 1 as usize))
        );
    }

    #[test]
    fn test_glr_chained_closure() {
        let start_symbol = String::from("S");
        let production_rules = vec![
            grammar::ProductionRule::new(
                "S".to_string(),
                vec![Symbol::NonTerminal("A".to_string())],
            ),
            grammar::ProductionRule::new(
                "A".to_string(),
                vec![Symbol::NonTerminal("B".to_string())],
            ),
            grammar::ProductionRule::new("B".to_string(), vec![Symbol::Terminal("c".to_string())]),
        ];
        let grammar = Grammar::new(
            start_symbol,
            vec!["c".to_string()],
            production_rules,
            Vec::new(),
        )
        .unwrap();

        let glr_parser = Glr::new(grammar).unwrap();
        let states = glr_parser.states;

        // The start state should contain the full closure:
        // S' -> .S
        // S -> .A
        // A -> .B
        // B -> .c
        let start_state = states.values().find(|s| s.is_accept_state).unwrap();

        assert!(
            start_state
                .transitions
                .contains_key(&Symbol::Terminal("c".to_string()))
        );
    }

    #[test]
    fn test_glr_shift_reduce_with_epsilon_conflict() {
        // Grammar:
        // S -> E F
        // E -> a
        // F -> b
        // F -> <epsilon>
        let start_symbol = String::from("S");
        let production_rules = vec![
            grammar::ProductionRule::new(
                "S".to_string(),
                vec![
                    Symbol::NonTerminal("E".to_string()),
                    Symbol::NonTerminal("F".to_string()),
                ],
            ),
            grammar::ProductionRule::new("E".to_string(), vec![Symbol::Terminal("a".to_string())]),
            grammar::ProductionRule::new("F".to_string(), vec![Symbol::Terminal("b".to_string())]),
            grammar::ProductionRule::new("F".to_string(), vec![]), // Epsilon rule
        ];
        let grammar = Grammar::new(
            start_symbol,
            vec!["a".to_string(), "b".to_string()],
            production_rules,
            Vec::new(),
        )
        .unwrap();

        let glr_parser = Glr::new(grammar).unwrap();
        let states = glr_parser.states;

        // A correct GLR parser will encounter a conflict after parsing `a`.
        // The state will contain items:
        // S -> E.F
        // F -> .b
        // F -> .
        // When the next token is `b`, two actions are possible:
        // 1. Shift 'b' to follow F -> .b
        // 2. Reduce `F -> <epsilon>` to follow S -> E.F and then S -> E F.
        // The state transition table for the state after parsing 'a' should have both a shift and a reduce action for the 'b' token.
        assert!(states.values().any(|s| {
            if s.transitions
                .contains_key(&Symbol::Terminal("b".to_string()))
            {
                s.reductions.contains(&("F".to_string(), 0))
            } else {
                false
            }
        }));
    }

    #[test]
    fn test_glr_left_recursion() {
        // Grammar:
        // E -> E + T
        // E -> T
        // T -> i
        let start_symbol = String::from("E");
        let production_rules = vec![
            grammar::ProductionRule::new(
                "E".to_string(),
                vec![
                    Symbol::NonTerminal("E".to_string()),
                    Symbol::Terminal("+".to_string()),
                    Symbol::NonTerminal("T".to_string()),
                ],
            ),
            grammar::ProductionRule::new(
                "E".to_string(),
                vec![Symbol::NonTerminal("T".to_string())],
            ),
            grammar::ProductionRule::new("T".to_string(), vec![Symbol::Terminal("i".to_string())]),
        ];
        let grammar = Grammar::new(
            start_symbol,
            vec!["+".to_string(), "i".to_string()],
            production_rules,
            Vec::new(),
        )
        .unwrap();

        let glr_parser = Glr::new(grammar).unwrap();
        let states = glr_parser.states;

        let start_state = states.values().find(|s| s.is_accept_state).unwrap();
        assert!((states.len() > 0) & (states.len() < 20));
        assert!(
            start_state
                .transitions
                .contains_key(&Symbol::Terminal("i".to_string()))
        );
        assert!(
            start_state
                .transitions
                .contains_key(&Symbol::NonTerminal("E".to_string()))
        );
        assert!(
            start_state
                .transitions
                .contains_key(&Symbol::NonTerminal("T".to_string()))
        );
    }
}
