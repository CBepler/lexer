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
