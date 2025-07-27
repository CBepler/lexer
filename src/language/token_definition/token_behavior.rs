use pair_definition::PairDefinition;

use crate::regex::Regex;

pub mod pair_definition;

#[derive(PartialEq, Debug)]
pub enum TokenBehavior {
    Ignore,
    Pair(PairDefinition),
    IgnoreUntil(String),
    IgnoreUntilCompiled(Regex),
    None,
}
