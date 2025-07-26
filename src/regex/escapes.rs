use std::vec;

#[derive(Clone, Debug, PartialEq, Copy, Eq, Hash)]
pub enum EscapeChar {
    WordBoundry,
    Digit,
    Whitespace,
    WordCharacter,
}

impl EscapeChar {
    pub fn matches_char(&self, c: char) -> bool {
        match self {
            EscapeChar::Digit => c.is_ascii_digit(),
            EscapeChar::Whitespace => c.is_ascii_whitespace(),
            EscapeChar::WordCharacter => c.is_ascii_alphanumeric() || c == '_',
            _ => false,
        }
    }

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

    pub fn all_variants() -> &'static [EscapeChar] {
        &[
            EscapeChar::WordBoundry,
            EscapeChar::Digit,
            EscapeChar::Whitespace,
            EscapeChar::WordCharacter,
        ]
    }
}
