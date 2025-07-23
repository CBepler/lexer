#[derive(Clone, Debug, PartialEq, Copy, Eq, Hash)]
pub enum EscapeChar {
    WordBoundry,
    Digit,
    Whitespace,
    WordCharacter,
}

impl EscapeChar {
    pub fn get_char_matches(&self) -> Vec<char> {
        match self {
            EscapeChar::Digit => ('0'..='9').collect(),
            EscapeChar::Whitespace => {
                vec![' ', '\t', '\n', '\r', '\x0B', '\x0C']
            }
            EscapeChar::WordCharacter => ('a'..='z')
                .chain('A'..='Z')
                .chain('0'..='9')
                .chain(std::iter::once('_'))
                .collect(),
            EscapeChar::WordBoundry => {
                Vec::new() // No characters directly match a word boundary
            }
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
