//! This module defines custom escape characters used within the regular expression engine.
//! It specifies how these escape characters behave, particularly concerning ASCII character matching.

use std::vec;

/// Represents different types of escape characters used in regular expressions.
#[derive(Clone, Debug, PartialEq, Copy, Eq, Hash)]
pub enum EscapeChar {
    /// Matches a word boundary (`\b`).
    ///
    /// Note: Its behavior is context-dependent
    /// and doesn't match a single character directly.
    WordBoundry,
    /// Matches an ASCII digit (`[0-9]`, equivalent to `\d`).
    Digit,
    /// Matches an ASCII whitespace character (`[\t\n\r\x0C\s ]`, equivalent to `\s`).
    Whitespace,
    /// Matches an ASCII word character (`[a-zA-Z0-9_]`, equivalent to `\w`).
    WordCharacter,
}

impl EscapeChar {
    /// Checks if a given character matches the criteria for this escape character.
    ///
    /// **Important:** As noted in the `README.md`, `\d`, `\s`, and `\w`
    /// currently only match **ASCII** characters. `\b` does not match characters directly.
    ///
    /// # Arguments
    /// * `c`: The character to check.
    ///
    /// # Returns
    /// `true` if `c` matches the escape character's criteria, `false` otherwise.
    pub fn matches_char(&self, c: char) -> bool {
        match self {
            EscapeChar::Digit => c.is_ascii_digit(),
            EscapeChar::Whitespace => c.is_ascii_whitespace(),
            EscapeChar::WordCharacter => c.is_ascii_alphanumeric() || c == '_',
            _ => false,
        }
    }

    /// Provides the ASCII character ranges that each escape character typically represents.
    ///
    /// This is useful for DFA construction or character set manipulation.
    /// `WordBoundry` returns an empty vector as it's a position-based assertion, not a character class.
    ///
    /// # Returns
    /// A `Vec` of `(start_char, end_char)` tuples representing the ASCII ranges.
    pub fn matching_ascii(&self) -> Vec<(char, char)> {
        match self {
            EscapeChar::Whitespace => vec![
                (' ', ' '),
                ('\t', '\t'),
                ('\r', '\r'),
                ('\n', '\n'),
                ('\u{000C}', '\u{000C}'),
            ],
            EscapeChar::Digit => vec![('0', '9')],
            EscapeChar::WordCharacter => vec![('a', 'z'), ('A', 'Z'), ('0', '9'), ('_', '_')],
            EscapeChar::WordBoundry => Vec::new(),
        }
    }

    /// Returns a slice containing all defined `EscapeChar` variants.
    pub fn all_variants() -> &'static [EscapeChar] {
        &[
            EscapeChar::WordBoundry,
            EscapeChar::Digit,
            EscapeChar::Whitespace,
            EscapeChar::WordCharacter,
        ]
    }
}
