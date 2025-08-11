use super::{NodeId, cst_node::CSTNode, state_constructor::IDConstructable};

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct GssNode {
    pub id: NodeId,
    pub state: Option<NodeId>,
    pub parents: Option<Vec<NodeId>>,
    pub cst_node: Option<CSTNode>,
}

impl IDConstructable for GssNode {
    fn new(id: NodeId) -> Self {
        GssNode {
            id,
            state: None,
            parents: None,
            cst_node: None,
        }
    }
}

impl GssNode {
    pub fn set_state(&mut self, state: NodeId) {
        self.state = Some(state);
    }

    pub fn set_parents(&mut self, parents: Vec<NodeId>) {
        self.parents = Some(parents);
    }

    pub fn set_cst_node(&mut self, cst_node: CSTNode) {
        self.cst_node = Some(cst_node);
    }
}
