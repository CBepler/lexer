use std::collections::{HashMap, HashSet, VecDeque};

use crate::{
    Parsable,
    parser::grammar::{Grammar, Symbol},
};

pub use self::cst_node::{CSTChildren, CSTNode};
use self::{
    gss_node::GssNode,
    state::{ProductionRule, State},
    state_constructor::StateConstructor,
};

pub mod cst_node;
mod gss_node;
mod state;
mod state_constructor;

pub type NodeId = usize;

pub struct Glr {
    grammar: Grammar,
    //Map from non-terminals to terminals that can start them
    firsts: HashMap<String, HashSet<String>>,
    //Map from non-terminals to terminals that can follow
    follows: HashMap<String, HashSet<String>>,
    states: HashMap<NodeId, State>,
    start_state_id: NodeId,
}

impl Glr {
    pub fn new(grammar: Grammar) -> Result<Self, String> {
        let (firsts, follows) = construct_firsts_follows(&grammar);
        let (states, start_id) = construct_states(&grammar);
        println!("States: {:?}", states);
        Ok(Glr {
            grammar,
            firsts,
            follows,
            states,
            start_state_id: start_id,
        })
    }

    pub fn parse<T, K>(&self, mut token_iter: T) -> Result<CSTNode, String>
    where
        T: Iterator<Item = Result<K, String>>,
        K: Parsable,
    {
        let mut gss_constructor = StateConstructor::new();
        let mut gss_map: HashMap<NodeId, GssNode> = HashMap::new();
        let mut gss_root = gss_constructor.new_state::<GssNode>();
        gss_root.set_state(self.start_state_id);
        gss_root.set_cst_node(CSTNode::new("S'".to_string(), None));
        let mut gss_roots: VecDeque<NodeId> = VecDeque::new();
        gss_roots.push_back(gss_root.id);
        gss_map.insert(gss_root.id, gss_root);
        while let Some(tok_res) = token_iter.next() {
            match tok_res {
                Ok(tok) => {
                    self.parse_token(&mut gss_roots, &mut gss_map, &mut gss_constructor, tok);
                }
                Err(e) => {
                    return Err(format!("Lexing Error: {e}"));
                }
            }
        }
        let final_root =
            self.handle_end_reductions(&mut gss_roots, &mut gss_map, &mut gss_constructor);
        println!("GSS Map: {:?}", gss_map);
        println!("___________________________________________________________________________");
        println!("GSS roots: {:?}", gss_roots);
        println!("___________________________________________________________________________");
        println!("States: {:?}", self.states);
        match final_root {
            Some(root) => {
                let root = gss_map.remove(&root).unwrap();
                Ok(root.cst_node.unwrap())
            }
            None => Err("Parsing failed: no valid parse found.".to_string()),
        }
    }

    fn parse_token<K>(
        &self,
        gss_roots: &mut VecDeque<usize>,
        gss_map: &mut HashMap<NodeId, GssNode>,
        gss_constructor: &mut StateConstructor,
        tok: K,
    ) where
        K: Parsable,
    {
        println!("Parsing Token: {:?}", tok.get_name());
        let mut next_gss_roots: VecDeque<NodeId> = VecDeque::new();
        while let Some(root_id) = gss_roots.pop_front() {
            println!("Root ID: {:?}", root_id);
            let root = gss_map.get(&root_id).unwrap();
            let current_state_id = root.state.unwrap();
            println!("Current State ID: {current_state_id}");
            let current_state = self.states.get(&current_state_id).unwrap();
            let mut terminals_to_process = Vec::new();
            if let Some(toks) = self.grammar.get_token_replacements(tok.get_name()) {
                terminals_to_process.extend(toks);
            } else {
                terminals_to_process.push(tok.get_name());
            }
            println!("Terminals to process: {:?}", terminals_to_process);
            println!("Current State: {:?}", current_state);
            //Need to handle shift-reduce ambiguities
            for terminal in terminals_to_process {
                self.process_shift(
                    current_state,
                    terminal,
                    &tok,
                    root_id,
                    &mut next_gss_roots,
                    gss_map,
                    gss_constructor,
                );
                println!("Next gss roots: {:?}", next_gss_roots);
                for reduction in &current_state.reductions {
                    self.process_reduction(
                        reduction,
                        Some(terminal),
                        root_id,
                        gss_map,
                        gss_constructor,
                        gss_roots,
                    );
                }
            }
        }
        println!("Next gss roots Final: {:?}", next_gss_roots);
        *gss_roots = next_gss_roots;
    }

    fn handle_end_reductions(
        &self,
        gss_roots: &mut VecDeque<usize>,
        gss_map: &mut HashMap<NodeId, GssNode>,
        gss_constructor: &mut StateConstructor,
    ) -> Option<NodeId> {
        println!("Handling End");
        let mut next_gss_roots: VecDeque<NodeId> = VecDeque::new();
        while let Some(root_id) = gss_roots.pop_front() {
            let root = gss_map.get(&root_id).unwrap();
            let current_state_id = root.state.unwrap();
            let current_state = self.states.get(&current_state_id).unwrap();
            println!("Root Id: {root_id}");
            println!("Current State: {:?}", current_state);
            for reduction in &current_state.reductions {
                self.process_reduction(
                    reduction,
                    None,
                    root_id,
                    gss_map,
                    gss_constructor,
                    gss_roots,
                );
            }
            if (current_state.is_accept_state) & !(next_gss_roots.contains(&root_id)) {
                next_gss_roots.push_back(root_id);
            }
            println!("Next gss roots: {:?}", next_gss_roots);
        }
        println!("Next gss roots Final: {:?}", next_gss_roots);
        if next_gss_roots.is_empty() {
            return None;
        }
        let mut final_root = gss_constructor.new_state::<GssNode>();
        final_root.cst_node = Some(CSTNode::new("S'".to_string(), None));
        final_root.cst_node.as_mut().unwrap().children = gss_map
            .get(&next_gss_roots.pop_front().unwrap())
            .unwrap()
            .cst_node
            .clone()
            .unwrap()
            .children;
        while let Some(id) = next_gss_roots.pop_front() {
            let cst_node = gss_map.get(&id).unwrap().cst_node.clone().unwrap();
            final_root.cst_node.as_mut().unwrap().children = Some(
                final_root
                    .cst_node
                    .clone()
                    .unwrap()
                    .children
                    .unwrap()
                    .combine(cst_node.children.unwrap()),
            );
        }
        let id = final_root.id;
        gss_map.insert(final_root.id, final_root);
        Some(id)
    }

    fn process_shift<K>(
        &self,
        current_state: &State,
        terminal: &String,
        tok: &K,
        root_id: usize,
        next_gss_roots: &mut VecDeque<NodeId>,
        gss_map: &mut HashMap<NodeId, GssNode>,
        gss_constructor: &mut StateConstructor,
    ) where
        K: Parsable,
    {
        if let Some(next_state_id) = current_state
            .transitions
            .get(&Symbol::Terminal(terminal.clone()))
        {
            //Need to check if the internals of another gss node in next roots already matches for consolidation
            let new_cst_node = CSTNode::new(terminal.clone(), tok.get_match().clone());
            let parents = vec![root_id];
            find_and_insert_gss_node(
                next_gss_roots,
                gss_map,
                next_state_id,
                new_cst_node,
                parents,
                gss_constructor,
            );
        }
    }

    fn process_reduction(
        &self,
        reduction: &(String, usize),
        terminal: Option<&String>,
        root_id: usize,
        gss_map: &mut HashMap<NodeId, GssNode>,
        gss_constructor: &mut StateConstructor,
        gss_roots: &mut VecDeque<NodeId>,
    ) {
        let to_reduce = match terminal {
            Some(terminal) => self.follows.get(&reduction.0).unwrap().contains(terminal),
            None => true,
        };
        if to_reduce {
            println!("Reduction: {:?}", reduction);
            let rule_len = reduction.1;
            println!("Rule Len: {}", rule_len);
            let (paths, path_cst_nodes) = find_reduction_paths(root_id, rule_len, gss_map);
            println!("Paths: {:?}", paths);
            println!("Path CST Nodes: {:?}", path_cst_nodes);
            for (idx, path) in paths.iter().enumerate() {
                let start_node_id = path.last().unwrap();
                let start_node = gss_map.get(start_node_id).unwrap();
                let start_state = self.states.get(&start_node.state.unwrap()).unwrap();
                let new_cst_node = CSTNode {
                    name: reduction.0.clone(),
                    value: None,
                    children: Some(CSTChildren::Single({
                        let mut arr =
                            path_cst_nodes[idx].clone()[0..(path_cst_nodes[0].len() - 1)].to_vec();
                        arr.reverse();
                        arr
                    })),
                };
                println!("New CST Node: {new_cst_node}");
                println!("Start State: {:?}", start_state);
                if let Some(next_state_id) = start_state
                    .transitions
                    .get(&Symbol::NonTerminal(reduction.0.clone()))
                {
                    let parents = vec![*start_node_id];
                    find_and_insert_gss_node(
                        gss_roots,
                        gss_map,
                        next_state_id,
                        new_cst_node,
                        parents,
                        gss_constructor,
                    );
                }
            }
        }
    }
}

fn find_existing_gss_node(
    gss_roots: &mut VecDeque<NodeId>,
    gss_map: &HashMap<NodeId, GssNode>,
    check_state_id: &NodeId,
    check_cst_node: &CSTNode,
) -> Option<NodeId> {
    for id in &*gss_roots {
        let gss_node = gss_map.get(&id).unwrap();
        if let Some(cst_node) = &gss_node.cst_node {
            if (gss_node.state == Some(*check_state_id)) & (cst_node == check_cst_node) {
                return Some(*id);
            }
        }
    }
    None
}

fn find_and_insert_gss_node(
    gss_roots: &mut VecDeque<NodeId>,
    gss_map: &mut HashMap<NodeId, GssNode>,
    check_state_id: &NodeId,
    new_cst_node: CSTNode,
    parents: Vec<NodeId>,
    gss_constructor: &mut StateConstructor,
) -> NodeId {
    match find_existing_gss_node(gss_roots, gss_map, &*check_state_id, &new_cst_node) {
        Some(id) => {
            let gss_node = gss_map.get_mut(&id).unwrap();
            println!("Gss Node before shift match: {:?}", gss_node);
            println!("New Parents: {:?}", parents);
            match &mut gss_node.parents {
                Some(node_parents) => {
                    node_parents.extend(parents.clone());
                    println!("Gss Node after shift match: {:?}", gss_node);
                }
                None => {
                    gss_node.set_parents(parents.clone());
                }
            }
            if new_cst_node.children.is_some() {
                match &mut gss_node.cst_node.as_mut().unwrap().children {
                    Some(children) => {
                        let children = children.clone();
                        gss_node.cst_node.as_mut().unwrap().children =
                            Some(children.combine(new_cst_node.children.unwrap()));
                    }
                    None => {
                        gss_node.cst_node.as_mut().unwrap().children =
                            new_cst_node.children.clone();
                    }
                }
            }
            id
        }
        None => {
            let mut new_gss_node = gss_constructor.new_state::<GssNode>();
            new_gss_node.set_state(*check_state_id);
            new_gss_node.set_parents(parents);
            new_gss_node.set_cst_node(new_cst_node.clone());
            gss_roots.push_back(new_gss_node.id);
            let id = new_gss_node.id;
            gss_map.insert(new_gss_node.id, new_gss_node);
            id
        }
    }
}

fn find_reduction_paths(
    end_node_id: NodeId,
    mut len: usize,
    gss_map: &HashMap<NodeId, GssNode>,
) -> (Vec<Vec<NodeId>>, Vec<Vec<CSTNode>>) {
    let mut paths: VecDeque<Vec<(NodeId, CSTNode)>> = VecDeque::new();
    println!("End Node ID: {end_node_id}");
    paths.push_back(vec![(
        end_node_id,
        gss_map.get(&end_node_id).unwrap().cst_node.clone().unwrap(),
    )]);
    println!("Initial Paths: {:?}", paths);
    while len > 0 {
        let mut next_paths: VecDeque<Vec<(NodeId, CSTNode)>> = VecDeque::new();
        while let Some(mut path) = paths.pop_back() {
            let (gss_node_id, _cst_node) = &path.last().unwrap();
            let gss_node = gss_map.get(gss_node_id).unwrap();
            let parent_node_id = gss_node.parents.as_ref().unwrap().first().unwrap();
            let parent_cst_node = gss_map
                .get(parent_node_id)
                .unwrap()
                .cst_node
                .clone()
                .unwrap();
            path.push((*parent_node_id, parent_cst_node));
            if gss_node.parents.as_ref().unwrap().len() > 1 {
                let parents = gss_node.parents.as_ref().unwrap()[1..].to_vec();
                for parent_node_id in parents {
                    let parent_cst_node = gss_map
                        .get(&parent_node_id)
                        .unwrap()
                        .cst_node
                        .clone()
                        .unwrap();
                    let mut new_path = path[0..(path.len() - 1)].to_vec();
                    new_path.push((parent_node_id, parent_cst_node));
                    next_paths.push_back(new_path);
                }
            }
            next_paths.push_back(path);
        }
        paths = next_paths;
        len -= 1;
    }
    let mut node_ids_vec: Vec<Vec<NodeId>> = Vec::new();
    let mut cst_nodes_vec: Vec<Vec<CSTNode>> = Vec::new();
    for inner_vec in paths {
        let mut inner_node_ids: Vec<NodeId> = Vec::new();
        let mut inner_cst_nodes: Vec<CSTNode> = Vec::new();
        for (node_id, cst_node) in inner_vec {
            inner_node_ids.push(node_id);
            inner_cst_nodes.push(cst_node);
        }
        node_ids_vec.push(inner_node_ids);
        cst_nodes_vec.push(inner_cst_nodes);
    }
    (node_ids_vec, cst_nodes_vec)
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
    follows.insert("S'".to_string(), HashSet::new());
    (firsts, follows)
}

fn construct_states(grammar: &Grammar) -> (HashMap<NodeId, State>, NodeId) {
    let mut production_map: HashMap<Vec<ProductionRule>, State> = HashMap::new();
    let mut state_constructor = StateConstructor::new();
    let mut start_state = state_constructor.new_state::<State>();
    let start_id = start_state.id;
    start_state.production_rules = get_closure(
        "S'",
        &vec![Symbol::NonTerminal(grammar.start_symbol.clone())],
        0,
        grammar,
    );

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
                let mut new_state = state_constructor.new_state::<State>();
                new_state.production_rules = rules;
                transitions.insert(symbol, new_state.id);
                states_to_process.push_back(new_state);
            }
        }
        state.transitions = transitions;
        state.reductions = reductions;
        production_map.insert(state.production_rules.clone(), state);
    }

    let mut state_map: HashMap<usize, State> = production_map
        .into_values()
        .map(|state| (state.id, state))
        .collect();
    let start_state = state_map.get_mut(&start_id).unwrap();
    let mut end_state = state_constructor.new_state::<State>();
    end_state.is_accept_state = true;
    start_state
        .transitions
        .insert(Symbol::NonTerminal("S'".to_string()), end_state.id);
    state_map.insert(end_state.id, end_state);
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

    #[derive(Debug, Clone)]
    struct MockToken {
        name: String,
        value: Option<String>,
    }

    impl Parsable for MockToken {
        fn get_name(&self) -> &String {
            &self.name
        }
        fn get_match(&self) -> &Option<String> {
            &self.value
        }

        fn get_col(&self) -> usize {
            0
        }

        fn get_row(&self) -> usize {
            0
        }
    }

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

        let start_state = states.get(&glr_parser.start_state_id).unwrap();
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
        assert!(
            start_state
                .transitions
                .contains_key(&Symbol::NonTerminal("S'".to_string()))
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
        let start_state = states.get(&glr_parser.start_state_id).unwrap();

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

        let start_state = states.get(&glr_parser.start_state_id).unwrap();
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

    #[test]
    fn test_parse_successful_unambiguous() {
        // Grammar: S -> A B, A -> a, B -> b
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

        let tokens = vec![
            Ok(MockToken {
                name: "a".to_string(),
                value: Some("a_val".to_string()),
            }),
            Ok(MockToken {
                name: "b".to_string(),
                value: Some("b_val".to_string()),
            }),
        ];

        let result = glr_parser.parse(tokens.into_iter());
        println!("{:?}", result);
        assert!(result.is_ok());

        let cst = result.unwrap();
        let expected_cst_children = CSTChildren::Single(vec![
            CSTNode {
                name: "A".to_string(),
                value: None,
                children: Some(CSTChildren::Single(vec![CSTNode::new(
                    "a".to_string(),
                    Some("a_val".to_string()),
                )])),
            },
            CSTNode {
                name: "B".to_string(),
                value: None,
                children: Some(CSTChildren::Single(vec![CSTNode::new(
                    "b".to_string(),
                    Some("b_val".to_string()),
                )])),
            },
        ]);
        let expected_cst_start = CSTChildren::Single(vec![CSTNode {
            name: "S".to_string(),
            value: None,
            children: Some(expected_cst_children),
        }]);
        let expected_cst = CSTNode {
            name: "S'".to_string(),
            value: None,
            children: Some(expected_cst_start),
        };
        assert_eq!(cst, expected_cst);
    }

    #[test]
    fn test_parse_ambiguous_grammar() {
        // Grammar: E -> E + E | id
        let start_symbol = String::from("E");
        let production_rules = vec![
            grammar::ProductionRule::new(
                "E".to_string(),
                vec![
                    Symbol::NonTerminal("E".to_string()),
                    Symbol::Terminal("+".to_string()),
                    Symbol::NonTerminal("E".to_string()),
                ],
            ),
            grammar::ProductionRule::new("E".to_string(), vec![Symbol::Terminal("id".to_string())]),
        ];
        let grammar = Grammar::new(
            start_symbol,
            vec!["+".to_string(), "id".to_string()],
            production_rules,
            Vec::new(),
        )
        .unwrap();
        let glr_parser = Glr::new(grammar).unwrap();

        let tokens = vec![
            Ok(MockToken {
                name: "id".to_string(),
                value: Some("x".to_string()),
            }),
            Ok(MockToken {
                name: "+".to_string(),
                value: Some("+".to_string()),
            }),
            Ok(MockToken {
                name: "id".to_string(),
                value: Some("y".to_string()),
            }),
            Ok(MockToken {
                name: "+".to_string(),
                value: Some("+".to_string()),
            }),
            Ok(MockToken {
                name: "id".to_string(),
                value: Some("z".to_string()),
            }),
        ];

        let result = glr_parser.parse(tokens.into_iter());
        assert!(result.is_ok());

        let cst = result.unwrap();
        println!("CST: {}", cst);

        // The final result should indicate ambiguity at the top level, on the S' node itself.
        assert_eq!(cst.name, "S'");
        assert!(cst.children.is_some());

        // The S' node's children should be the ambiguous paths directly.
        let ambiguous_paths = match cst.children.as_ref().unwrap() {
            CSTChildren::Ambiguous(paths) => paths,
            _ => panic!("Expected Ambiguous children for S' node"),
        };
        assert_eq!(ambiguous_paths.len(), 2);

        // Helper closure to check for the left-associative tree structure
        let is_left_associative = |path: &Vec<CSTNode>| -> bool {
            // Path should have one top-level E node for S' -> E
            if path.len() != 1 || path[0].name != "E" {
                return false;
            }

            let Some(CSTChildren::Single(nodes)) = path[0].children.as_ref() else {
                return false;
            };

            // The children of this E should be E, +, E
            if nodes.len() != 3
                || nodes[0].name != "E"
                || nodes[1].name != "+"
                || nodes[2].name != "E"
            {
                return false;
            }

            // The first E should represent (x + y)
            let Some(CSTChildren::Single(nodes_inner)) = nodes[0].children.as_ref() else {
                return false;
            };
            if nodes_inner.len() != 3
                || nodes_inner[0].name != "E"
                || nodes_inner[1].name != "+"
                || nodes_inner[2].name != "E"
            {
                return false;
            }

            // Check for the leaf nodes 'x' and 'y'
            let Some(CSTChildren::Single(leaf_x)) = nodes_inner[0].children.as_ref() else {
                return false;
            };
            if leaf_x.len() != 1 || leaf_x[0].value != Some("x".to_string()) {
                return false;
            }

            let Some(CSTChildren::Single(leaf_y)) = nodes_inner[2].children.as_ref() else {
                return false;
            };
            if leaf_y.len() != 1 || leaf_y[0].value != Some("y".to_string()) {
                return false;
            }

            // The last E should represent 'z'
            let Some(CSTChildren::Single(leaf_z)) = nodes[2].children.as_ref() else {
                return false;
            };
            if leaf_z.len() != 1 || leaf_z[0].value != Some("z".to_string()) {
                return false;
            }

            true
        };

        // Helper closure to check for the right-associative tree structure
        let is_right_associative = |path: &Vec<CSTNode>| -> bool {
            // Path should have one top-level E node for S' -> E
            if path.len() != 1 || path[0].name != "E" {
                return false;
            }

            let Some(CSTChildren::Single(nodes)) = path[0].children.as_ref() else {
                return false;
            };

            // The children of this E should be E, +, E
            if nodes.len() != 3
                || nodes[0].name != "E"
                || nodes[1].name != "+"
                || nodes[2].name != "E"
            {
                return false;
            }

            // The first E should represent 'x'
            let Some(CSTChildren::Single(leaf_x)) = nodes[0].children.as_ref() else {
                return false;
            };
            if leaf_x.len() != 1 || leaf_x[0].value != Some("x".to_string()) {
                return false;
            }

            // The last E should represent (y + z)
            let Some(CSTChildren::Single(nodes_inner)) = nodes[2].children.as_ref() else {
                return false;
            };
            if nodes_inner.len() != 3
                || nodes_inner[0].name != "E"
                || nodes_inner[1].name != "+"
                || nodes_inner[2].name != "E"
            {
                return false;
            }

            // Check for the leaf nodes 'y' and 'z'
            let Some(CSTChildren::Single(leaf_y)) = nodes_inner[0].children.as_ref() else {
                return false;
            };
            if leaf_y.len() != 1 || leaf_y[0].value != Some("y".to_string()) {
                return false;
            }

            let Some(CSTChildren::Single(leaf_z)) = nodes_inner[2].children.as_ref() else {
                return false;
            };
            if leaf_z.len() != 1 || leaf_z[0].value != Some("z".to_string()) {
                return false;
            }

            true
        };

        // The order of the paths is not guaranteed, so we check for the existence of both forms.
        let mut found_left = false;
        let mut found_right = false;

        for path in ambiguous_paths {
            if is_left_associative(path) {
                found_left = true;
            } else if is_right_associative(path) {
                found_right = true;
            }
        }

        println!("Found Left: {found_left}");
        println!("Found Right: {found_right}");

        assert!(
            found_left && found_right,
            "Both left and right associative parse trees were not found."
        );
    }

    #[test]
    fn test_parse_failure_invalid_input() {
        // Grammar: S -> a b
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

        // Invalid input: 'a' followed by 'a'
        let tokens = vec![
            Ok(MockToken {
                name: "a".to_string(),
                value: Some("a_val1".to_string()),
            }),
            Ok(MockToken {
                name: "a".to_string(),
                value: Some("a_val2".to_string()),
            }),
        ];

        let result = glr_parser.parse(tokens.into_iter());
        assert!(result.is_err());
        assert_eq!(
            result.unwrap_err(),
            "Parsing failed: no valid parse found.".to_string()
        );
    }

    #[test]
    fn test_parse_ambiguous_tokens() {
        // Grammar:
        // S -> Decl | Assign
        // Decl -> TYPE_DEF
        // Assign -> VAR
        let start_symbol = String::from("S");
        let production_rules = vec![
            grammar::ProductionRule::new(
                "S".to_string(),
                vec![Symbol::NonTerminal("Decl".to_string())],
            ),
            grammar::ProductionRule::new(
                "S".to_string(),
                vec![Symbol::NonTerminal("Assign".to_string())],
            ),
            grammar::ProductionRule::new(
                "Decl".to_string(),
                vec![Symbol::Terminal("TYPE_DEF".to_string())],
            ),
            grammar::ProductionRule::new(
                "Assign".to_string(),
                vec![Symbol::Terminal("VAR".to_string())],
            ),
        ];

        let ambiguous_tokens = vec![grammar::AmbiguousToken::new(
            String::from("IDENTIFIER"),
            vec![String::from("VAR"), String::from("TYPE_DEF")],
        )];

        let grammar = Grammar::new(
            start_symbol,
            vec!["IDENTIFIER".to_string()], // Base token name
            production_rules,
            ambiguous_tokens,
        )
        .unwrap();
        let glr_parser = Glr::new(grammar).unwrap();

        let tokens = vec![Ok(MockToken {
            name: "IDENTIFIER".to_string(),
            value: Some("my_variable".to_string()),
        })];

        let result = glr_parser.parse(tokens.into_iter());
        assert!(result.is_ok());

        let cst = result.unwrap();
        println!("CST: {}", cst);

        assert_eq!(cst.name, "S'");
        assert!(cst.children.is_some());

        // The S' node's children should be the ambiguous paths directly.
        let ambiguous_paths = match cst.children.as_ref().unwrap() {
            CSTChildren::Ambiguous(paths) => paths,
            _ => panic!("Expected Ambiguous children for S' node"),
        };
        assert_eq!(ambiguous_paths.len(), 2);

        let mut found_decl = false;
        let mut found_assign = false;

        for path in ambiguous_paths {
            if path.len() != 1 || path[0].name != "S" {
                continue;
            }

            let Some(CSTChildren::Single(s_children)) = path[0].children.as_ref() else {
                continue;
            };

            // S's child should be either Decl or Assign
            if s_children.len() != 1 {
                continue;
            }

            if s_children[0].name == "Decl" {
                let Some(CSTChildren::Single(decl_children)) = s_children[0].children.as_ref()
                else {
                    continue;
                };
                if decl_children.len() == 1
                    && decl_children[0].name == "TYPE_DEF"
                    && decl_children[0].value == Some("my_variable".to_string())
                {
                    found_decl = true;
                }
            } else if s_children[0].name == "Assign" {
                let Some(CSTChildren::Single(assign_children)) = s_children[0].children.as_ref()
                else {
                    continue;
                };
                if assign_children.len() == 1
                    && assign_children[0].name == "VAR"
                    && assign_children[0].value == Some("my_variable".to_string())
                {
                    found_assign = true;
                }
            }
        }

        println!("Found Decl: {found_decl}");
        println!("Found Assign: {found_assign}");

        assert!(
            found_decl && found_assign,
            "Both Decl and Assign parse trees were not found."
        );
    }

    #[test]
    fn test_glr_complex_encompassing_grammar() {
        // Grammar: simple arithmetic with parentheses and addition/multiplication
        // Also has ambiguity in expression parsing without precedence rules
        let start_symbol = String::from("Expr");
        let production_rules = vec![
            // Expr -> Expr + Term | Term
            grammar::ProductionRule::new(
                "Expr".to_string(),
                vec![
                    Symbol::NonTerminal("Expr".to_string()),
                    Symbol::Terminal("+".to_string()),
                    Symbol::NonTerminal("Term".to_string()),
                ],
            ),
            grammar::ProductionRule::new(
                "Expr".to_string(),
                vec![Symbol::NonTerminal("Term".to_string())],
            ),
            // Term -> Term * Factor | Factor
            grammar::ProductionRule::new(
                "Term".to_string(),
                vec![
                    Symbol::NonTerminal("Term".to_string()),
                    Symbol::Terminal("*".to_string()),
                    Symbol::NonTerminal("Factor".to_string()),
                ],
            ),
            grammar::ProductionRule::new(
                "Term".to_string(),
                vec![Symbol::NonTerminal("Factor".to_string())],
            ),
            // Factor -> ( Expr ) | number
            grammar::ProductionRule::new(
                "Factor".to_string(),
                vec![
                    Symbol::Terminal("(".to_string()),
                    Symbol::NonTerminal("Expr".to_string()),
                    Symbol::Terminal(")".to_string()),
                ],
            ),
            grammar::ProductionRule::new(
                "Factor".to_string(),
                vec![Symbol::Terminal("num".to_string())],
            ),
        ];

        let grammar = Grammar::new(
            start_symbol,
            vec![
                "+".to_string(),
                "*".to_string(),
                "(".to_string(),
                ")".to_string(),
                "num".to_string(),
            ],
            production_rules,
            Vec::new(),
        )
        .unwrap();

        let glr_parser = Glr::new(grammar).unwrap();

        // Input: (1 + 2) * 3 + 4
        let tokens = vec![
            MockToken {
                name: "(".to_string(),
                value: Some("(".to_string()),
            },
            MockToken {
                name: "num".to_string(),
                value: Some("1".to_string()),
            },
            MockToken {
                name: "+".to_string(),
                value: Some("+".to_string()),
            },
            MockToken {
                name: "num".to_string(),
                value: Some("2".to_string()),
            },
            MockToken {
                name: ")".to_string(),
                value: Some(")".to_string()),
            },
            MockToken {
                name: "*".to_string(),
                value: Some("*".to_string()),
            },
            MockToken {
                name: "num".to_string(),
                value: Some("3".to_string()),
            },
            MockToken {
                name: "+".to_string(),
                value: Some("+".to_string()),
            },
            MockToken {
                name: "num".to_string(),
                value: Some("4".to_string()),
            },
        ];

        let result = glr_parser.parse(tokens.into_iter().map(Ok));

        assert!(result.is_ok(), "Parser failed on complex grammar");
        let cst = result.unwrap();

        // This isn't asserting the full tree shape — just verifying the root matches the start symbol
        assert_eq!(cst.name, "S'");
        assert!(cst.children.is_some(), "Parse tree has no children");

        // You could add debug prints here if you want to see the CST:
        println!("{:#?}", cst);
    }
}
