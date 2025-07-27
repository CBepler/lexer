#[macro_export]
macro_rules! __lexor_create_token_definition {
    ($name:expr, $regex_str:expr, $priority:expr, $to_store:expr, $behavior:expr) => {
        $crate::language::TokenDefinition::new(
            $name.to_string(),
            $regex_str,
            $behavior,
            $priority,
            $to_store,
        )
    };
}

#[macro_export]
macro_rules! keyword {
    ($name:expr, $regex_str:expr, $priority:expr) => {
        $crate::__lexor_create_token_definition!(
            $name,
            $regex_str,
            $priority,
            false,
            $crate::language::TokenBehavior::None
        )
    };
}

#[macro_export]
macro_rules! token {
    ($name:expr, $regex_str:expr, $priority:expr, $to_store:expr) => {
        $crate::__lexor_create_token_definition!(
            $name,
            $regex_str,
            $priority,
            $to_store,
            $crate::language::TokenBehavior::None
        )
    };
}

#[macro_export]
macro_rules! ignore_token {
    ($name:expr, $regex_str:expr, $priority:expr) => {
        $crate::__lexor_create_token_definition!(
            $name,
            $regex_str,
            $priority,
            false,
            $crate::language::TokenBehavior::Ignore
        )
    };
}

#[macro_export]
macro_rules! open_pair {
    ($name:expr, $regex_str:expr, $counterpart:expr, $priority:expr) => {
        $crate::__lexor_create_token_definition!(
            $name,
            $regex_str,
            $priority,
            false,
            $crate::language::TokenBehavior::Pair($crate::language::PairDefinition::new(
                $crate::language::PairDirection::Open,
                $counterpart.to_string(),
            ))
        )
    };
}

#[macro_export]
macro_rules! close_pair {
    ($name:expr, $regex_str:expr, $counterpart:expr, $priority:expr) => {
        $crate::__lexor_create_token_definition!(
            $name,
            $regex_str,
            $priority,
            false,
            $crate::language::TokenBehavior::Pair($crate::language::PairDefinition::new(
                $crate::language::PairDirection::Close,
                $counterpart.to_string(),
            ))
        )
    };
}

#[macro_export]
macro_rules! ignore_until {
    ($name:expr, $regex_str:expr, $end_regex_str:expr, $priority:expr) => {
        $crate::__lexor_create_token_definition!(
            $name,
            $regex_str,
            $priority,
            false,
            $crate::language::TokenBehavior::IgnoreUntil($end_regex_str.to_string())
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
    use crate::language::{PairDefinition, PairDirection, TokenBehavior, TokenDefinition};

    fn create_expected_token(
        name: &str,
        regex_str: &str,
        priority: i32,
        to_store_match: bool,
        behavior: TokenBehavior,
    ) -> TokenDefinition {
        TokenDefinition::new(
            name.to_string(),
            regex_str,
            behavior, // Behavior is now the 3rd argument in TokenDefinition::new
            priority,
            to_store_match,
        )
        .expect("test failed in creating token definition")
    }

    #[test]
    fn test_keyword_macro() {
        let result = keyword!("TEST_KEYWORD", r"test", 100);
        let expected =
            create_expected_token("TEST_KEYWORD", r"test", 100, false, TokenBehavior::None);
        assert_eq!(result.unwrap(), expected);
    }

    #[test]
    fn test_token_macro() {
        let result = token!("TEST_IDENT", r"[a-z]+", 50, true);
        let expected =
            create_expected_token("TEST_IDENT", r"[a-z]+", 50, true, TokenBehavior::None);
        assert_eq!(result.unwrap(), expected);
    }

    #[test]
    fn test_ignore_token_macro() {
        let result = ignore_token!("WHITESPACE", r"\s+", 10);
        let expected =
            create_expected_token("WHITESPACE", r"\s+", 10, false, TokenBehavior::Ignore);
        assert_eq!(result.unwrap(), expected);
    }

    #[test]
    fn test_open_pair_macro() {
        let result = open_pair!("L_BRACE", r"\{", "R_BRACE", 90);
        let expected = create_expected_token(
            "L_BRACE",
            r"\{",
            90,
            false,
            TokenBehavior::Pair(PairDefinition::new(
                PairDirection::Open,
                "R_BRACE".to_string(),
            )),
        );
        assert_eq!(result.unwrap(), expected);
    }

    #[test]
    fn test_close_pair_macro() {
        let result = close_pair!("R_BRACE", r"\}", "L_BRACE", 90);
        let expected = create_expected_token(
            "R_BRACE",
            r"\}",
            90,
            false,
            TokenBehavior::Pair(PairDefinition::new(
                PairDirection::Close,
                "L_BRACE".to_string(),
            )),
        );
        assert_eq!(result.unwrap(), expected);
    }

    #[test]
    fn test_ignore_until_macro() {
        let result = ignore_until!("MULTI_COMMENT", r"/\*", r"\*/", 5);
        let expected = create_expected_token(
            "MULTI_COMMENT",
            r"/\*",
            5,
            false,
            TokenBehavior::IgnoreUntil(r"\*/".to_string()),
        );
        assert_eq!(result.unwrap(), expected);
    }

    #[test]
    fn test_define_language_macro_success() {
        let module_token = keyword!("MODULE", r"^module\b", 100);
        let identifier_token = token!("IDENTIFIER", r"^[a-zA-Z_][a-zA-Z0-9_]*", 50, true);
        let open_paren_token = open_pair!("LEFT_PAREN", r"^\(", "RIGHT_PAREN", 90);
        let close_paren_token = close_pair!("RIGHT_PAREN", r"^\)", "LEFT_PAREN", 90);

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
