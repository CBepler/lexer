#[macro_export]
macro_rules! __lexor_create_token_definition {
    ($name:expr, $regex_str:expr, $behavior:expr) => {
        $crate::language::TokenDefinition::new($name.to_string(), $regex_str, $behavior)
    };
}

#[macro_export]
macro_rules! keyword {
    ($name:expr, $regex_str:expr) => {
        $crate::__lexor_create_token_definition!(
            $name,
            $regex_str,
            $crate::language::TokenBehavior::Keyword
        )
    };
}

#[macro_export]
macro_rules! store_token {
    ($name:expr, $regex_str:expr) => {
        $crate::__lexor_create_token_definition!(
            $name,
            $regex_str,
            $crate::language::TokenBehavior::Store
        )
    };
}

#[macro_export]
macro_rules! ignore_token {
    ($name:expr, $regex_str:expr) => {
        $crate::__lexor_create_token_definition!(
            $name,
            $regex_str,
            $crate::language::TokenBehavior::Ignore
        )
    };
}

#[macro_export]
macro_rules! open_pair {
    ($name:expr, $regex_str:expr, $counterpart:expr) => {
        $crate::__lexor_create_token_definition!(
            $name,
            $regex_str,
            $crate::language::TokenBehavior::Pair($crate::language::PairDefinition::new(
                $crate::language::PairDirection::Open,
                $counterpart.to_string(),
            ))
        )
    };
}

#[macro_export]
macro_rules! close_pair {
    ($name:expr, $regex_str:expr, $counterpart:expr) => {
        $crate::__lexor_create_token_definition!(
            $name,
            $regex_str,
            $crate::language::TokenBehavior::Pair($crate::language::PairDefinition::new(
                $crate::language::PairDirection::Close,
                $counterpart.to_string(),
            ))
        )
    };
}

#[macro_export]
macro_rules! single_line_comment {
    ($name:expr, $regex_str:expr) => {
        $crate::__lexor_create_token_definition!(
            $name,
            $regex_str,
            $crate::language::TokenBehavior::CommentStart(
                $crate::language::CommentEndCondition::Newline,
            )
        )
    };
}

#[macro_export]
macro_rules! multi_line_comment {
    ($name:expr, $start_regex_str:expr, $end_regex_str:expr) => {
        $crate::__lexor_create_token_definition!(
            $name,
            $start_regex_str,
            $crate::language::TokenBehavior::CommentStart(
                $crate::language::CommentEndCondition::RegexStr($end_regex_str.to_string()),
            )
        )
    };
}

#[macro_export]
macro_rules! define_language {
    ( $( $token_def:expr ),* $(,)? ) => {
        $crate::language::Language::new_from_results(
            vec![ $( $token_def ),* ],
        )
    };
}

#[cfg(test)]
mod tests {
    use crate::language::{
        CommentEndCondition, PairDefinition, PairDirection, TokenBehavior, TokenDefinition,
    };
    use regex::Regex;

    fn create_expected_token(
        name: &str,
        regex_str: &str,
        behavior: TokenBehavior,
    ) -> TokenDefinition {
        TokenDefinition::new(name.to_string(), regex_str, behavior)
            .expect("test failed in creating token definition")
    }

    #[test]
    fn test_keyword_macro() {
        let result = keyword!("TEST_KEYWORD", r"test");
        let expected = create_expected_token("TEST_KEYWORD", r"test", TokenBehavior::Keyword);
        assert_eq!(result.unwrap(), expected);
    }

    #[test]
    fn test_store_token_macro() {
        let result = store_token!("TEST_IDENT", r"[a-z]+");
        let expected = create_expected_token("TEST_IDENT", r"[a-z]+", TokenBehavior::Store);
        assert_eq!(result.unwrap(), expected);
    }

    #[test]
    fn test_ignore_token_macro() {
        let result = ignore_token!("WHITESPACE", r"\s+");
        let expected = create_expected_token("WHITESPACE", r"\s+", TokenBehavior::Ignore);
        assert_eq!(result.unwrap(), expected);
    }

    #[test]
    fn test_open_pair_macro() {
        let result = open_pair!("L_BRACE", r"\{", "R_BRACE");
        let expected = create_expected_token(
            "L_BRACE",
            r"\{",
            TokenBehavior::Pair(PairDefinition::new(
                PairDirection::Open,
                "R_BRACE".to_string(),
            )),
        );
        assert_eq!(result.unwrap(), expected);
    }

    #[test]
    fn test_close_pair_macro() {
        let result = close_pair!("R_BRACE", r"\}", "L_BRACE");
        let expected = create_expected_token(
            "R_BRACE",
            r"\}",
            TokenBehavior::Pair(PairDefinition::new(
                PairDirection::Close,
                "L_BRACE".to_string(),
            )),
        );
        assert_eq!(result.unwrap(), expected);
    }

    #[test]
    fn test_single_line_comment_macro() {
        let result = single_line_comment!("SINGLE_COMMENT", r"//.*");
        let expected = create_expected_token(
            "SINGLE_COMMENT",
            r"//.*",
            TokenBehavior::CommentStart(CommentEndCondition::Newline),
        );
        assert_eq!(result.unwrap(), expected);
    }

    #[test]
    fn test_multi_line_comment_macro() {
        let result = multi_line_comment!("MULTI_COMMENT", r"/\*", r"\*/");
        let expected = create_expected_token(
            "MULTI_COMMENT",
            r"/\*",
            TokenBehavior::CommentStart(CommentEndCondition::Regex(Regex::new(r"\*/").unwrap())),
        );
        assert_eq!(result.unwrap(), expected);
    }

    #[test]
    fn test_define_language_macro_success() {
        let module_token = keyword!("MODULE", r"^module\b");
        let identifier_token = store_token!("IDENTIFIER", r"^[a-zA-Z_][a-zA-Z0-9_]*");
        let open_paren_token = open_pair!("LEFT_PAREN", r"^\(", "RIGHT_PAREN");
        let close_paren_token = close_pair!("RIGHT_PAREN", r"^\)", "LEFT_PAREN");

        let language_result = define_language! {
            module_token,
            identifier_token,
            open_paren_token,
            close_paren_token,
        };

        assert!(language_result.is_ok());
        let language = language_result.unwrap();
        let definitions = language.get_token_definitions();

        assert_eq!(definitions.len(), 4);
        assert!(definitions.iter().any(|t| t.get_name() == "MODULE"));
        assert!(definitions.iter().any(|t| t.get_name() == "IDENTIFIER"));
        assert!(definitions.iter().any(|t| t.get_name() == "LEFT_PAREN"));
        assert!(definitions.iter().any(|t| t.get_name() == "RIGHT_PAREN"));
    }
}
