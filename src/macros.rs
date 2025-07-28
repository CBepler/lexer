#[macro_export]
/// Internal macro used by other token definition macros to create a `TokenDefinition`.
///
/// This macro provides a consistent way to construct `TokenDefinition` instances
/// by delegating to `TokenDefinition::new`. It's not intended for direct use.
///
/// # Arguments
/// * `$name`: The name of the token as a string literal.
/// * `$regex_str`: The regular expression pattern for the token as a string literal.
/// * `$priority`: An integer representing the token's priority.
/// * `$to_store`: A boolean indicating whether to store the matched text.
/// * `$behavior`: The `TokenBehavior` for the token.
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
/// Creates a `TokenDefinition` for a **keyword**.
///
/// Keywords are typically reserved words in a language. This macro sets
/// `to_store_match` to `false` and `TokenBehavior` to `None` by default.
///
/// # Arguments
/// * `$name`: The name of the keyword token (e.g., "IF_KEYWORD").
/// * `$regex_str`: The regex pattern for the keyword (e.g., `r"if\b"` for word boundary).
/// * `$priority`: The priority of the keyword.
///
/// # Returns
/// A `Result<TokenDefinition, String>` which can be unwrapped to get the definition
/// if the regex is valid.
///
/// # Examples
/// ```
/// use lexit::keyword;
/// let if_token = keyword!("IF_KEYWORD", r"if\b", 100);
/// assert!(if_token.is_ok());
/// ```
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
/// Creates a generic `TokenDefinition`.
///
/// This macro allows full control over whether the matched text should be stored.
/// The `TokenBehavior` is set to `None` by default.
///
/// # Arguments
/// * `$name`: The name of the token (e.g., "IDENTIFIER").
/// * `$regex_str`: The regex pattern for the token.
/// * `$priority`: The priority of the token.
/// * `$to_store`: A boolean; `true` to store the matched text, `false` otherwise.
///
/// # Returns
/// A `Result<TokenDefinition, String>`.
///
/// # Examples
/// ```
/// use lexit::token;
/// let identifier = token!("IDENTIFIER", r"[a-zA-Z_][a-zA-Z0-9_]*", 50, true);
/// assert!(identifier.is_ok());
/// ```
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
/// Creates a `TokenDefinition` for a token that should be **ignored** by the lexer.
///
/// Ignored tokens (like whitespace) are recognized but
/// not included in the final token stream. `to_store_match` is always `false`.
///
/// # Arguments
/// * `$name`: The name of the ignored token (e.g., "WHITESPACE").
/// * `$regex_str`: The regex pattern for the ignored token.
/// * `$priority`: The priority of the ignored token.
///
/// # Returns
/// A `Result<TokenDefinition, String>`.
///
/// # Examples
/// ```
/// use lexit::ignore_token;
/// let whitespace = ignore_token!("WHITESPACE", r"\s+", 10);
/// assert!(whitespace.is_ok());
/// ```
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
/// Creates a `TokenDefinition` for an **opening paired token** (e.g., `(` or `{`).
///
/// Paired tokens help the lexer track balanced delimiters. `to_store_match` is
/// always `false`.
///
/// # Arguments
/// * `$name`: The name of the opening pair token (e.g., "LEFT_PAREN").
/// * `$regex_str`: The regex pattern for the opening pair.
/// * `$counterpart`: The name of its corresponding closing pair token (e.g., "RIGHT_PAREN").
/// * `$priority`: The priority of the token.
///
/// # Returns
/// A `Result<TokenDefinition, String>`.
///
/// # Examples
/// ```
/// use lexit::open_pair;
/// let left_brace = open_pair!("LEFT_BRACE", r"\{", "RIGHT_BRACE", 90);
/// assert!(left_brace.is_ok());
/// ```
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
/// Creates a `TokenDefinition` for a **closing paired token** (e.g., `)` or `}`).
///
/// Paired tokens help the lexer track balanced delimiters. `to_store_match` is
/// always `false`.
///
/// # Arguments
/// * `$name`: The name of the closing pair token (e.g., "RIGHT_PAREN").
/// * `$regex_str`: The regex pattern for the closing pair.
/// * `$counterpart`: The name of its corresponding opening pair token (e.g., "LEFT_PAREN").
/// * `$priority`: The priority of the token.
///
/// # Returns
/// A `Result<TokenDefinition, String>`.
///
/// # Examples
/// ```
/// use lexit::close_pair;
/// let right_brace = close_pair!("RIGHT_BRACE", r"\}", "LEFT_BRACE", 90);
/// assert!(right_brace.is_ok());
/// ```
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
/// Creates a `TokenDefinition` for a token that marks the **start of an ignored block**
/// until a specific end regex is matched.
///
/// This is typically used for multi-line comments. The content between the start
/// token and the end regex is ignored. `to_store_match` is always `false`.
///
/// # Arguments
/// * `$name`: The name of the ignore-until token (e.g., "MULTI_LINE_COMMENT").
/// * `$regex_str`: The regex pattern that starts the ignored block (e.g., `r"/\*" `).
/// * `$end_regex_str`: The regex pattern that ends the ignored block (e.g., `r"\*/" `).
/// * `$priority`: The priority of the token.
///
/// # Returns
/// A `Result<TokenDefinition, String>`.
///
/// # Examples
/// ```
/// use lexit::ignore_until;
/// let multi_comment = ignore_until!("MULTI_LINE_COMMENT", r"/\*", r"\*/", 5);
/// assert!(multi_comment.is_ok());
/// ```
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
/// Defines a complete `Language` by providing a list of `TokenDefinition` results.
///
/// This macro simplifies the creation of a `Language` object by taking a
/// comma-separated list of `Result<TokenDefinition, String>` expressions
/// (typically generated by other token macros like `keyword!`, `token!`, etc.).
/// It collects these into a `Vec` and uses `Language::new_from_results` for validation
/// and final `Language` creation.
///
/// # Arguments
/// * `$( $token_def:expr ),*`: A comma-separated list of expressions that evaluate to
///   `Result<TokenDefinition, String>`.
///
/// # Returns
/// A `Result<Language, String>`. If any of the provided token definitions
/// are errors, or if the resulting `Language` definition is invalid (e.g.,
/// duplicate token names, unmatched pairs), an `Err` will be returned.
///
/// # Examples
/// ```
/// use lexit::{define_language, keyword, token, open_pair, close_pair};
///
/// let my_language = define_language! {
///     keyword!("FN", r"fn\b", 100),
///     token!("IDENT", r"[a-z_]+", 90, true),
///     open_pair!("L_PAREN", r"\(", "R_PAREN", 80),
///     close_pair!("R_PAREN", r"\)", "L_PAREN", 80),
/// };
///
/// assert!(my_language.is_ok());
/// let language = my_language.unwrap();
/// assert_eq!(language.get_token_definitions().len(), 4);
/// ```
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
            behavior,
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
