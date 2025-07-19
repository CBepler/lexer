use comment_end_condition::CommentEndCondition;
use pair_definition::PairDefinition;

pub mod comment_end_condition;
pub mod pair_definition;

#[derive(PartialEq, Debug)]
pub enum TokenBehavior {
    Ignore,
    Keyword,
    Pair(PairDefinition),
    Store,
    CommentStart(CommentEndCondition),
}
