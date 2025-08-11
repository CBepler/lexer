use std::fmt::{self, Display, Formatter};

//Concrete Syntax Tree
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct CSTNode {
    pub name: String,
    pub value: Option<String>,
    pub children: Option<CSTChildren>,
}

impl CSTNode {
    pub fn new(name: String, value: Option<String>) -> Self {
        CSTNode {
            name,
            value,
            children: None,
        }
    }
}

impl Display for CSTNode {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        // Start the formatting with the root node at indentation level 0.
        format_node_with_indent(f, self, 0)
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum CSTChildren {
    Single(Vec<CSTNode>),
    Ambiguous(Vec<Vec<CSTNode>>),
}

impl CSTChildren {
    pub fn is_single(&self) -> bool {
        if let CSTChildren::Single(_) = self {
            return true;
        }
        false
    }

    pub fn combine(self, other: Self) -> Self {
        let mut children = Vec::new();
        match self {
            CSTChildren::Single(child) => {
                children.push(child);
            }
            CSTChildren::Ambiguous(a_children) => {
                children.extend(a_children);
            }
        }
        match other {
            CSTChildren::Single(child) => {
                children.push(child);
            }
            CSTChildren::Ambiguous(a_children) => {
                children.extend(a_children);
            }
        }
        CSTChildren::Ambiguous(children)
    }
}

fn format_node_with_indent(f: &mut Formatter, node: &CSTNode, indent: usize) -> fmt::Result {
    write!(f, "{:indent$}", "", indent = indent * 4)?;

    match &node.value {
        Some(val) => writeln!(f, "- {}: {}", node.name, val)?,
        None => writeln!(f, "- {}", node.name)?,
    };

    if let Some(children) = &node.children {
        match children {
            CSTChildren::Single(nodes) => {
                for child in nodes {
                    format_node_with_indent(f, child, indent + 2)?;
                }
            }
            CSTChildren::Ambiguous(ambiguous_nodes) => {
                writeln!(f, "{:indent$}  (Ambiguous Paths):", "", indent = indent * 4)?;
                for (i, path) in ambiguous_nodes.iter().enumerate() {
                    writeln!(f, "{:indent$}    Path {}:", "", i)?;
                    for child in path {
                        format_node_with_indent(f, child, indent + 2)?;
                    }
                }
            }
        }
    }
    Ok(())
}
