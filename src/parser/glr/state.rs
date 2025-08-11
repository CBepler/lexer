use std::collections::{HashMap, HashSet};

use super::{NodeId, state_constructor::IDConstructable};
use crate::parser::grammar::Symbol;

#[derive(Debug)]
pub struct State {
    pub id: NodeId,
    //terminal or non-terminal to next state
    pub transitions: HashMap<Symbol, NodeId>,
    //terminal to reductions with (non-terminal name, body length)
    pub reductions: HashSet<(String, usize)>,
    pub production_rules: Vec<ProductionRule>,
    pub is_accept_state: bool,
}

impl IDConstructable for State {
    fn new(id: NodeId) -> Self {
        State {
            id,
            transitions: HashMap::new(),
            reductions: HashSet::new(),
            production_rules: Vec::new(),
            is_accept_state: false,
        }
    }
}

#[derive(Hash, PartialEq, Eq, Clone, Debug)]
pub struct ProductionRule {
    pub head: String,
    pub body: Vec<Symbol>,
    pub pos: usize,
}

impl ProductionRule {
    pub fn new(head: String, body: Vec<Symbol>, pos: usize) -> Self {
        ProductionRule { head, body, pos }
    }
}
