use pair_definition::PairDefinition;

use crate::regex::Regex;

pub mod pair_definition;

/// Defines the specific behavior associated with a token after it has been matched by the lexer.
#[derive(PartialEq, Debug)]
pub enum TokenBehavior {
    /// The token should be ignored and not included in the final stream of tokens.
    /// This is typically used for whitespace
    Ignore,
    /// The token is part of a pair (e.g., `(` and `)`).
    /// It carries information about its direction (open/close) and its counterpart's name.
    Pair(PairDefinition),
    /// The token signals the start of a block of text that should be ignored until a
    /// specific end pattern is encountered. The end pattern is provided as a string regex.
    IgnoreUntil(String),
    /// Similar to `IgnoreUntil`, but the end pattern has already been compiled into a `Regex` object.
    /// This variant is used internally after `TokenDefinition::new` has processed `IgnoreUntil(String)`.
    IgnoreUntilCompiled(Regex),
    /// The token has no special behavior and should be processed as a standard token.
    None,
}
