/// Defines the properties of a paired token, such as parentheses or braces.
#[derive(PartialEq, Debug)]
pub struct PairDefinition {
    /// The direction of the pair (e.g., `Open` for `(`, `Close` for `)`).
    pair_type: PairDirection,
    /// The name of the token that acts as the counterpart to this token.
    /// For an `Open` pair, this is the name of its `Close` counterpart.
    /// For a `Close` pair, this is the name of its `Open` counterpart.
    counterpart_name: String,
}

impl PairDefinition {
    /// Creates a new `PairDefinition`.
    ///
    /// # Arguments
    /// * `pair_type`: The `PairDirection` of this token (e.g., `PairDirection::Open`).
    /// * `counterpart_name`: The name of the token that forms the other half of the pair.
    ///
    /// # Examples
    /// ```
    /// use lexer::language::{PairDefinition, PairDirection};
    ///
    /// let open_paren_def = PairDefinition::new(PairDirection::Open, "RIGHT_PAREN".to_string());
    /// assert_eq!(open_paren_def.get_pair_type(), &PairDirection::Open);
    /// assert_eq!(open_paren_def.get_counterpart_name(), "RIGHT_PAREN");
    /// ```
    pub fn new(pair_type: PairDirection, counterpart_name: String) -> Self {
        PairDefinition {
            pair_type,
            counterpart_name,
        }
    }

    /// Returns a reference to the `PairDirection` of this token.
    pub fn get_pair_type(&self) -> &PairDirection {
        &self.pair_type
    }

    /// Returns the name of the counterpart token.
    pub fn get_counterpart_name(&self) -> &str {
        &self.counterpart_name
    }
}

/// Specifies whether a paired token is an opening or closing delimiter.
#[derive(PartialEq, Debug)]
pub enum PairDirection {
    /// Indicates an opening paired token (e.g., `(` or `{`).
    Open,
    /// Indicates a closing paired token (e.g., `)` or `}`).
    Close,
}
