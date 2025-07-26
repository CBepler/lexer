use crate::regex::Regex;

#[derive(Debug)]
pub enum CommentEndCondition {
    Newline,
    RegexStr(String),
    Regex(Regex),
}

impl PartialEq for CommentEndCondition {
    fn eq(&self, other: &Self) -> bool {
        match self {
            CommentEndCondition::Newline => matches!(other, CommentEndCondition::Newline),
            CommentEndCondition::RegexStr(x) => match other {
                CommentEndCondition::RegexStr(y) => x == y,
                _ => false,
            },
            CommentEndCondition::Regex(x) => match other {
                CommentEndCondition::Regex(y) => x.to_string() == y.to_string(),
                _ => false,
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn comment_end_condition_newline_equality_asserts_eq() {
        let c1 = CommentEndCondition::Newline;
        let c2 = CommentEndCondition::Newline;
        assert_eq!(c1, c2);
    }

    #[test]
    fn comment_end_condition_regexstr_equality_asserts_eq() {
        let c1 = CommentEndCondition::RegexStr(r"\(".to_string());
        let c2 = CommentEndCondition::RegexStr(r"\(".to_string());
        assert_eq!(c1, c2);
    }

    #[test]
    fn comment_end_condition_regexstr_equality_asserts_ne() {
        let c1 = CommentEndCondition::RegexStr(r"\(".to_string());
        let c2 = CommentEndCondition::RegexStr(r"\)".to_string());
        assert_ne!(c1, c2);
    }

    #[test]
    fn comment_end_condition_regex_equality_asserts_eq() {
        let c1 = CommentEndCondition::Regex(Regex::new(r"\(").unwrap());
        let c2 = CommentEndCondition::Regex(Regex::new(r"\(").unwrap());
        assert_eq!(c1, c2);
    }

    #[test]
    fn comment_end_condition_regex_equality_asserts_ne() {
        let c1 = CommentEndCondition::Regex(Regex::new(r"\(").unwrap());
        let c2 = CommentEndCondition::Regex(Regex::new(r"\)").unwrap());
        assert_ne!(c1, c2);
    }
}
