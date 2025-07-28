### `lexer`

A configurable and robust lexical analyzer (lexer) library for Rust. This crate provides a powerful macro-based approach to defining programming language lexers with support for keywords, operators, identifiers, literals, whitespace, and comments, including intricate features like paired delimiters and multi-line comments.

---

### Getting Started

Add lexer to your project with `cargo add lexer`

### Defining a language

Languages creation is streamlined with the "define_language!" macro and the smaller token creation macros including:
* `token!(token_name, regular expression match, priority, whether to store and return the text that matches the token)`
* `keyword!(token_name, regular expression, priority)` #Keywords are meant for set keywords, so storing the match would be unnecessary
* `ignore_token!(token_name, regex, priority)` #This is for matches you want to catch and then ignore. These are not returned as tokens and usually used to ignore whitespace
* `open_pair!(token_name, regex, counterpart_name, priority)` #This is used to define token pairs that must exist together like '(' and ')'. The language will return an error if you do not define a closing for an open pair. The lexer will error if there are any unclosed open pairs
* `close_pair!(token_name, regex, couterpart_name, priority)` #This is the counterpart to open pair. Likewise, the language will error if there is no matching open pair and the lexer will error if there is a random closing pair without matching open pair
* `ignore_until!(token_name, regex, ending_regex, priority)` #This is generally used for comments. It says "When I match on regex, I will ignore all characters until I match on ending_regex, and then I will resume regular activity"

Below is an example of defining a small subset of the C language:

```rust
let language_result = define_language! {
        ignore_token!("WHITESPACE", r"\s+", 10),

        ignore_until!("SINGLE_LINE_COMMENT", r"//", r"\n", 5),
        ignore_until!("MULTI_LINE_COMMENT", r"/\*", r"\*/", 5),

        keyword!("INT_KEYWORD", r"\bint\b", 100),
        keyword!("IF_KEYWORD", r"\bif\b", 100),
        keyword!("ELSE_KEYWORD", r"\belse\b", 100),
        keyword!("WHILE_KEYWORD", r"\bwhile\b", 100),
        keyword!("RETURN_KEYWORD", r"\breturn\b", 100),
        keyword!("VOID_KEYWORD", r"\bvoid\b", 100),

        keyword!("EQUALS_COMP", r"==", 95),
        keyword!("LESS_THAN_COMP", r"<", 90),
        keyword!("ASSIGN", r"=", 90),
        keyword!("PLUS", r"\+", 90),
        keyword!("MINUS", r"-", 90),
        keyword!("MULTIPLY", r"\*", 90),
        keyword!("DIVIDE", r"/", 90),


        open_pair!("LEFT_PAREN", r"\(", "RIGHT_PAREN", 80),
        close_pair!("RIGHT_PAREN", r"\)", "LEFT_PAREN", 80),
        open_pair!("LEFT_BRACE", r"\{", "RIGHT_BRACE", 80),
        close_pair!("RIGHT_BRACE", r"\}", "LEFT_BRACE", 80),

        keyword!("SEMICOLON", r";", 70),
        keyword!("COMMA", r",", 70),

        token!("IDENTIFIER", r"[a-zA-Z_][a-zA-Z0-9_]*", 60, true),

        token!("INTEGER_LITERAL", r"\d+", 60, true),
    };
```

The use of macros is optional and only offers conciseness. Here is an example of defining a simple arithmetic language without macros:

```rust
let definitions = vec![
        TokenDefinition::new(
            "WHITESPACE".to_string(),
            r"\s+",
            TokenBehavior::Ignore,
            0,
            false,
        )
        .unwrap(),
        TokenDefinition::new("PLUS".to_string(), r"\+", TokenBehavior::None, 50, false).unwrap(),
        TokenDefinition::new("MINUS".to_string(), r"-", TokenBehavior::None, 50, false).unwrap(),
        TokenDefinition::new(
            "MULTIPLY".to_string(),
            r"\*",
            TokenBehavior::None,
            50,
            false,
        )
        .unwrap(),
        TokenDefinition::new("DIVIDE".to_string(), r"/", TokenBehavior::None, 50, false).unwrap(),
        TokenDefinition::new(
            "LEFT_PAREN".to_string(),
            r"\(",
            TokenBehavior::Pair(PairDefinition::new(
                PairDirection::Open,
                "RIGHT_PAREN".to_string(),
            )),
            60,
            false,
        )
        .unwrap(),
        TokenDefinition::new(
            "RIGHT_PAREN".to_string(),
            r"\)",
            TokenBehavior::Pair(PairDefinition::new(
                PairDirection::Close,
                "LEFT_PAREN".to_string(),
            )),
            60,
            false,
        )
        .unwrap(),
        TokenDefinition::new(
            "FLOAT_LITERAL".to_string(),
            r"\d+\.\d+",
            TokenBehavior::None,
            70,
            true,
        )
        .unwrap(),
        TokenDefinition::new(
            "INTEGER_LITERAL".to_string(),
            r"\d+",
            TokenBehavior::None,
            65,
            true,
        )
        .unwrap(),
    ];

    let language = Language::new(definitions);
```




### Regular Expressions

In order to implement the dfa construction for lexing, I implemented my own regular expression logic. It supports the common regex patterns including:
* **Literals**: `'a'`
* **Quantifiers**: `'a{2, 3}'` `'a*'` `'a+'`
* **Range**: `'[a-c]'`
* **Not-Range**: `'[^a-c]'`
* **Groups**: `'(a)'`
* **Concatenation**: `'ab'`
* **Alternation**: `'a|b'`
* **StartAnchor**: `'^a'` #Start anchor means it must be at the start of a line
* **EndAnchor**: `'a$'` #End anchor means it must be at the end of a line
* **Any Character**: `'.'`
* **Escape Characters**:
  * `\b`: word boundry
  * `\d`: ascii-digit
  * `\s`: ascii-whitespace
  * `\w`: ascii-word_character

Note that while the regular expressions suport the full unicode character set, the escape characters \d, \s, and \w only work for ascii

---

### Tokens

A lexer is created by calling `Lexer::new(language)`. This will create the dfa for that language and then you can lex with `lexer.lex(text)`. The lex method returns a `Result<Vec<Token>, String>`. Tokens have 4 fields:

* `name`: `String` #The token name
* `text_match`: `Option<String>` #The matching text if store is enabled
* `row`: `usize` #The row in the text of the token
* `col`: `usize` #The column in the text of the token

### Lexing

Below is an example of lexing text:

```rust
    let lexer = Lexer::new(language.unwrap()).unwrap();
    let input_string = "(1 + 2.5) * 3";
    let tokens_result = lexer.lex(input_string);
```