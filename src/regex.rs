use escapes::EscapeChar;
use std::iter::Peekable;

pub mod escapes;

#[derive(Clone, Debug, PartialEq)]
pub enum RegexPattern {
    Literal(char),
    Quantifier(Box<RegexPattern>, usize, Option<usize>),
    NotRange(Vec<RangeType>),
    Group(Box<RegexPattern>),
    Range(Vec<RangeType>),
    Concatenation(Vec<Box<RegexPattern>>),
    Alternation(Vec<Box<RegexPattern>>),
    StartAnchor,
    EndAnchor,
    AnyCharacter,
    EscapeChar(EscapeChar),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum RangeType {
    SingleChar(char),
    MultiChar(char, char),
    SingleEscape(EscapeChar),
}

impl RangeType {
    pub fn contains(&self, c: char) -> bool {
        match self {
            RangeType::SingleChar(ch) => *ch == c,
            RangeType::MultiChar(low, high) => (*low <= c) & (*high >= c),
            RangeType::SingleEscape(e) => e.matches_char(c),
        }
    }
}

#[derive(Debug)]
pub struct Regex {
    pattern: RegexPattern,
    original_string: String,
}

impl Regex {
    pub fn new(text: &str) -> Result<Self, String> {
        let mut text_iter = text.chars().into_iter().peekable();
        let pattern = parse_expression(&mut text_iter)?;
        match text_iter.next() {
            Some(x) => Err(format!("Invalid end of grouping without start: {x}")),
            None => Ok(Regex {
                pattern,
                original_string: text.to_string(),
            }),
        }
    }

    pub fn get_pattern(&self) -> &RegexPattern {
        &self.pattern
    }

    pub fn to_string(&self) -> &str {
        &self.original_string
    }
}

fn parse_expression<I>(text: &mut Peekable<I>) -> Result<RegexPattern, String>
where
    I: Iterator<Item = char>,
{
    let mut terms = Vec::new();
    match text.peek() {
        Some(_) => (),
        None => return Err("Unexpected end of regex after start of expression".to_string()),
    }
    loop {
        let term_res = parse_term(text);
        match term_res {
            Ok(term) => terms.push(Box::new(term)),
            Err(e) => return Err(e),
        }
        let next_op = text.peek();
        match next_op {
            Some('|') => (),
            Some(')') | None => break,
            Some(_) => break,
        }
        text.next();
    }
    if terms.is_empty() {
        return Err("Unexpected end of input".to_string());
    }
    if terms.len() == 1 {
        Ok(*terms.pop().unwrap())
    } else {
        Ok(RegexPattern::Alternation(terms))
    }
}

fn parse_term<I>(text: &mut Peekable<I>) -> Result<RegexPattern, String>
where
    I: Iterator<Item = char>,
{
    let mut factors = Vec::new();
    match text.peek() {
        Some('^') => {
            factors.push(Box::new(RegexPattern::StartAnchor));
            text.next();
        }
        Some(_) | None => (),
    }
    loop {
        let next_op = text.peek();
        match next_op {
            Some('|') | Some(')') | None => break,
            Some(_) => (),
        };
        let factor_res = parse_factor(text);
        match factor_res {
            Ok(factor) => factors.push(Box::new(factor)),
            Err(e) => return Err(e),
        }
    }
    if factors.len() == 0 {
        return Err("Unexpected empty factor".to_string());
    }
    if factors.len() == 1 {
        return Ok(*factors[0].clone());
    }
    Ok(RegexPattern::Concatenation(factors))
}

fn parse_factor<I>(text: &mut Peekable<I>) -> Result<RegexPattern, String>
where
    I: Iterator<Item = char>,
{
    let atom_res = parse_atom(text);
    let atom = match atom_res {
        Ok(x) => x,
        Err(e) => return Err(e),
    };
    let next_op = text.peek();
    let full_atom = match next_op {
        Some('{') => {
            text.next();
            parse_quantifier(text, atom)?
        }
        Some('?') => {
            text.next();
            RegexPattern::Quantifier(Box::new(atom), 0, Some(1))
        }
        Some('*') => {
            text.next();
            RegexPattern::Quantifier(Box::new(atom), 0, None)
        }
        Some('+') => {
            text.next();
            RegexPattern::Quantifier(Box::new(atom), 1, None)
        }
        Some(_) | None => atom,
    };
    Ok(full_atom)
}

fn parse_atom<I>(text: &mut Peekable<I>) -> Result<RegexPattern, String>
where
    I: Iterator<Item = char>,
{
    let next_op = text.peek();
    match next_op {
        Some('(') => {
            text.next();
            let group = parse_expression(text)?;
            let Some(')') = text.next() else {
                return Err("Unclosed group: Expected ')'".to_string());
            };
            Ok(RegexPattern::Group(Box::new(group)))
        }
        Some('[') => {
            text.next();
            let range = construct_range(text)?;
            Ok(range)
        }
        Some('$') => {
            text.next();
            match text.peek() {
                Some(')') | Some('|') | None => Ok(RegexPattern::EndAnchor),
                Some(x) => Err(format!("Invalid end anchor before: {x}")),
            }
        }
        Some('.') => {
            text.next();
            Ok(RegexPattern::AnyCharacter)
        }
        Some('\\') => {
            text.next();
            parse_escape(text)
        }
        Some(c) if !".*+?()[]{}|^$\\".contains(*c) => {
            let ch = c.clone();
            text.next();
            Ok(RegexPattern::Literal(ch))
        }
        Some(c) => Err(format!("Unexpected character: '{c}'")),
        None => Err("Unexpected end of input while parsing atom".to_string()),
    }
}

fn parse_escape<I>(text: &mut Peekable<I>) -> Result<RegexPattern, String>
where
    I: Iterator<Item = char>,
{
    let escaped_char = text
        .next()
        .ok_or_else(|| "Nothing followed \\".to_string())?;
    match escaped_char {
        'b' => Ok(RegexPattern::EscapeChar(EscapeChar::WordBoundry)),
        'd' => Ok(RegexPattern::EscapeChar(EscapeChar::Digit)),
        's' => Ok(RegexPattern::EscapeChar(EscapeChar::Whitespace)),
        'w' => Ok(RegexPattern::EscapeChar(EscapeChar::WordCharacter)),
        'n' => Ok(RegexPattern::Literal('\n')),
        '*' | '?' | '\\' | '(' | ')' | '[' | ']' | '{' | '}' | '^' | '$' | '|' | '.' | '+' => {
            Ok(RegexPattern::Literal(escaped_char))
        }
        '\'' => Ok(RegexPattern::Literal('\'')),
        '"' => Ok(RegexPattern::Literal('"')),
        x => Err(format!("Invalid escape sequence: \\{x}")),
    }
}

fn parse_quantifier<I>(text: &mut Peekable<I>, atom: RegexPattern) -> Result<RegexPattern, String>
where
    I: Iterator<Item = char>,
{
    let min_str = parse_digits(text)?;
    let min: usize = min_str
        .parse()
        .map_err(|_| format!("Invalid min quantifier: {min_str}"))?;

    let next_char = text
        .next()
        .ok_or_else(|| "Unclosed quantifier: Expected '}' or ','".to_string())?;

    let max: Option<usize>;
    if next_char == ',' {
        let after_comma_char = text.peek().copied();
        if after_comma_char == Some('}') {
            max = None;
        } else {
            let max_str = parse_digits(text)?;
            max = Some(
                max_str
                    .parse()
                    .map_err(|_| format!("Invalid max quantifier: {max_str}"))?,
            );
        }
        let Some('}') = text.next() else {
            return Err("Unclosed quantifier: Expected '}'".to_string());
        };
    } else if next_char == '}' {
        max = Some(min);
    } else {
        return Err(format!("Unexpected character in quantifier: '{next_char}'"));
    }

    Ok(RegexPattern::Quantifier(Box::new(atom), min, max))
}

fn parse_digits<I>(text: &mut Peekable<I>) -> Result<String, String>
where
    I: Iterator<Item = char>,
{
    let mut num_str = String::new();
    let digit_ender;
    loop {
        match text.peek() {
            Some('0'..='9') => {
                num_str.push(text.next().unwrap());
            }
            x => {
                digit_ender = x;
                break;
            }
        }
    }
    if num_str.is_empty() {
        Err(format!(
            "Expected digits for quantifier but instead first got: {:?}",
            digit_ender
        ))
    } else {
        Ok(num_str)
    }
}

fn construct_range<I>(text: &mut Peekable<I>) -> Result<RegexPattern, String>
where
    I: Iterator<Item = char>,
{
    match text.peek() {
        Some('^') => {
            text.next();
            let range = parse_range(text)?;
            Ok(RegexPattern::NotRange(range))
        }
        Some(_) | None => {
            let range = parse_range(text)?;
            Ok(RegexPattern::Range(range))
        }
    }
}

fn parse_range<I>(text: &mut Peekable<I>) -> Result<Vec<RangeType>, String>
where
    I: Iterator<Item = char>,
{
    let mut start_range: Option<char> = None;
    let mut ranges = Vec::new();
    let mut last_char: Option<char> = None;
    loop {
        let next_op = text.next();
        match next_op {
            Some(']') => {
                if start_range != None {
                    ranges.push(RangeType::SingleChar(start_range.unwrap()));
                    ranges.push(RangeType::SingleChar('-'));
                }
                break;
            }
            Some('-') => {
                if start_range != None {
                    return Err("Invalid --".to_string());
                }
                if last_char == None {
                    ranges.push(RangeType::SingleChar('-'))
                } else {
                    start_range = last_char;
                }
            }
            Some('\\') => {
                if start_range != None {
                    start_range = None;
                    ranges.push(RangeType::SingleChar(last_char.unwrap()));
                    ranges.push(RangeType::SingleChar('-'));
                }
                let escape = parse_escape(text)?;
                match escape {
                    RegexPattern::Literal(x) => ranges.push(RangeType::SingleChar(x)),
                    RegexPattern::EscapeChar(x) => {
                        if x == EscapeChar::WordBoundry {
                            return Err(
                                "Word Boundry escape character is not allowed in range".to_string()
                            );
                        }
                        ranges.push(RangeType::SingleEscape(x));
                    }
                    _ => return Err(format!("Invalid return from parse_escape {:?}", escape)),
                }
                last_char = None;
            }
            Some(x) => {
                if start_range != None {
                    let start = start_range.unwrap();
                    if (start as u32) < (x as u32) {
                        ranges.push(RangeType::MultiChar(start_range.unwrap(), x));
                        last_char = None;
                    } else {
                        ranges.push(RangeType::SingleChar(start));
                        ranges.push(RangeType::SingleChar('-'));
                        last_char = check_single(text, &mut ranges, x);
                    }
                    start_range = None;
                } else {
                    last_char = check_single(text, &mut ranges, x);
                }
            }
            None => return Err("Invalid end of range".to_string()),
        }
    }
    Ok(ranges)
}

fn check_single<I>(
    text: &mut Peekable<I>,
    ranges: &mut Vec<RangeType>,
    current: char,
) -> Option<char>
where
    I: Iterator<Item = char>,
{
    match text.peek() {
        Some('-') => Some(current),
        Some(_) => {
            ranges.push(RangeType::SingleChar(current));
            None
        }
        None => None,
    }
}

#[cfg(test)]
mod tests {
    use super::{EscapeChar::*, RangeType::*, RegexPattern::*, *};

    fn get_reg(text: &str) -> Result<Regex, String> {
        let reg_res = Regex::new(text);
        match reg_res {
            Ok(ref x) => {
                println!("{:?}", x.get_pattern());
            }
            Err(ref e) => {
                println!("{}", e);
            }
        };
        reg_res
    }

    #[test]
    fn regex_simple() {
        let reg = get_reg("abcd").unwrap();
        let ans = Concatenation(vec![
            Box::new(Literal('a')),
            Box::new(Literal('b')),
            Box::new(Literal('c')),
            Box::new(Literal('d')),
        ]);
        assert_eq!(reg.get_pattern(), &ans);
    }

    #[test]
    fn regex_simple_alternation() {
        let reg = get_reg("a|b").unwrap();
        let ans = Alternation(vec![Box::new(Literal('a')), Box::new(Literal('b'))]);
        assert_eq!(reg.get_pattern(), &ans);
    }

    #[test]
    fn regex_simple_group() {
        let reg = get_reg("a(b|c)d").unwrap();
        let ans = Concatenation(vec![
            Box::new(Literal('a')),
            Box::new(Group(Box::new(Alternation(vec![
                Box::new(Literal('b')),
                Box::new(Literal('c')),
            ])))),
            Box::new(Literal('d')),
        ]);
        assert_eq!(reg.get_pattern(), &ans);
    }

    #[test]
    fn regex_quantifier() {
        let reg = get_reg("a{2,4}").unwrap();
        let ans = Quantifier(Box::new(Literal('a')), 2, Some(4));
        assert_eq!(reg.get_pattern(), &ans);
    }

    #[test]
    fn regex_quantifier_kleene_star() {
        let reg = get_reg("a*").unwrap();
        let ans = Quantifier(Box::new(Literal('a')), 0, None);
        assert_eq!(reg.get_pattern(), &ans);
    }

    #[test]
    fn regex_quantifier_plus() {
        let reg = get_reg("a+").unwrap();
        let ans = Quantifier(Box::new(Literal('a')), 1, None);
        assert_eq!(reg.get_pattern(), &ans);
    }

    #[test]
    fn regex_quantifier_no_second_is_none() {
        let reg = get_reg("a{0,}").unwrap();
        let ans = Quantifier(Box::new(Literal('a')), 0, None);
        assert_eq!(reg.get_pattern(), &ans);
    }

    #[test]
    fn regex_start_anchor() {
        let reg = get_reg("^a").unwrap();
        let ans = Concatenation(vec![Box::new(StartAnchor), Box::new(Literal('a'))]);
        assert_eq!(reg.get_pattern(), &ans);
    }

    #[test]
    fn regex_end_anchor() {
        let reg = get_reg("^a$").unwrap();
        let ans = Concatenation(vec![
            Box::new(StartAnchor),
            Box::new(Literal('a')),
            Box::new(EndAnchor),
        ]);
        assert_eq!(reg.get_pattern(), &ans);
    }

    #[test]
    fn regex_any_character() {
        let reg = get_reg(".").unwrap();
        let ans = AnyCharacter;
        assert_eq!(reg.get_pattern(), &ans);
    }

    #[test]
    fn regex_escapes() {
        let reg = get_reg(r"\w\s\d\b").unwrap();
        let ans = Concatenation(vec![
            Box::new(EscapeChar(WordCharacter)),
            Box::new(EscapeChar(Whitespace)),
            Box::new(EscapeChar(Digit)),
            Box::new(EscapeChar(WordBoundry)),
        ]);
        assert_eq!(reg.get_pattern(), &ans);
    }

    #[test]
    fn regex_range_simple() {
        let reg = get_reg(r"[a-z]").unwrap();
        let ans = Range(vec![MultiChar('a', 'z')]);
        assert_eq!(reg.get_pattern(), &ans);
    }

    #[test]
    fn regex_range_multiple_ranges() {
        let reg = get_reg(r"[a-zA-Z0-9]").unwrap();
        let ans = Range(vec![
            MultiChar('a', 'z'),
            MultiChar('A', 'Z'),
            MultiChar('0', '9'),
        ]);
        assert_eq!(reg.get_pattern(), &ans);
    }

    #[test]
    fn regex_range_single_value() {
        let reg = get_reg(r"[a-zA-Z0-9_]").unwrap();
        let ans = Range(vec![
            MultiChar('a', 'z'),
            MultiChar('A', 'Z'),
            MultiChar('0', '9'),
            SingleChar('_'),
        ]);
        assert_eq!(reg.get_pattern(), &ans);
    }

    #[test]
    fn regex_range_following_hypen() {
        let reg = get_reg(r"[a-d-z_]").unwrap();
        let ans = Range(vec![
            MultiChar('a', 'd'),
            SingleChar('-'),
            SingleChar('z'),
            SingleChar('_'),
        ]);
        assert_eq!(reg.get_pattern(), &ans);
    }

    #[test]
    fn regex_range_escape_characters() {
        let reg = get_reg(r"[\s\w\da0-9]").unwrap();
        let ans = Range(vec![
            SingleEscape(Whitespace),
            SingleEscape(WordCharacter),
            SingleEscape(Digit),
            SingleChar('a'),
            MultiChar('0', '9'),
        ]);
        assert_eq!(reg.get_pattern(), &ans);
    }

    #[test]
    fn regex_range_word_bounry_errors() {
        let reg = get_reg(r"[\b]");
        assert!(reg.is_err());
    }

    #[test]
    fn regex_range_backwards_to_single() {
        let reg = get_reg(r"[a-\sz-a]").unwrap();
        let ans = Range(vec![
            SingleChar('a'),
            SingleChar('-'),
            SingleEscape(Whitespace),
            SingleChar('z'),
            SingleChar('-'),
            SingleChar('a'),
        ]);
        assert_eq!(reg.get_pattern(), &ans);
    }

    #[test]
    fn regex_complex_varity() {
        let reg = get_reg(r"^[a-z]+|(01|\d.\s\w\b)$").unwrap();
        let ans = Alternation(vec![
            Box::new(Concatenation(vec![
                Box::new(StartAnchor),
                Box::new(Quantifier(
                    Box::new(Range(vec![MultiChar('a', 'z')])),
                    1,
                    None,
                )),
            ])),
            Box::new(Concatenation(vec![
                Box::new(Group(Box::new(Alternation(vec![
                    Box::new(Concatenation(vec![
                        Box::new(Literal('0')),
                        Box::new(Literal('1')),
                    ])),
                    Box::new(Concatenation(vec![
                        Box::new(EscapeChar(Digit)),
                        Box::new(AnyCharacter),
                        Box::new(EscapeChar(Whitespace)),
                        Box::new(EscapeChar(WordCharacter)),
                        Box::new(EscapeChar(WordBoundry)),
                    ])),
                ])))),
                Box::new(EndAnchor),
            ])),
        ]);
        assert_eq!(reg.get_pattern(), &ans);
    }

    #[test]
    fn regex_not_range() {
        let reg = get_reg(r"[^a0-9]").unwrap();
        let ans = NotRange(vec![SingleChar('a'), MultiChar('0', '9')]);
        assert_eq!(reg.get_pattern(), &ans);
    }

    #[test]
    fn regex_anchors_complex() {
        let reg = get_reg(r"^a|^bcd$|^f(^gh$)*$").unwrap();
        let ans = Alternation(vec![
            Box::new(Concatenation(vec![
                Box::new(StartAnchor),
                Box::new(Literal('a')),
            ])),
            Box::new(Concatenation(vec![
                Box::new(StartAnchor),
                Box::new(Literal('b')),
                Box::new(Literal('c')),
                Box::new(Literal('d')),
                Box::new(EndAnchor),
            ])),
            Box::new(Concatenation(vec![
                Box::new(StartAnchor),
                Box::new(Literal('f')),
                Box::new(Quantifier(
                    Box::new(Group(Box::new(Concatenation(vec![
                        Box::new(StartAnchor),
                        Box::new(Literal('g')),
                        Box::new(Literal('h')),
                        Box::new(EndAnchor),
                    ])))),
                    0,
                    None,
                )),
                Box::new(EndAnchor),
            ])),
        ]);
        assert_eq!(reg.get_pattern(), &ans);
    }

    #[test]
    fn regex_invalid_start_anchor() {
        let reg = get_reg(r"a^b");
        assert!(reg.is_err());
    }

    #[test]
    fn regex_invalid_end_anchor() {
        let reg = get_reg(r"a$b");
        assert!(reg.is_err());
    }

    #[test]
    fn regex_unclosed_group() {
        assert!(get_reg("(a|b").is_err());
        assert!(get_reg("a(b").is_err());
        assert!(get_reg("((a)").is_err());
        assert!(get_reg("(a|").is_err());
    }

    #[test]
    fn regex_unopened_group() {
        assert!(get_reg("a)b").is_err());
        assert!(get_reg("a)").is_err());
    }

    #[test]
    fn regex_empty_expression() {
        assert!(get_reg("").is_err());
        assert!(get_reg("()").is_err());
        assert!(get_reg("a|").is_err());
        assert!(get_reg("|a").is_err());
        assert!(get_reg("||").is_err());
    }

    #[test]
    fn regex_quantifier_no_atom() {
        assert!(get_reg("*a").is_err());
        assert!(get_reg("+b").is_err());
        assert!(get_reg("?c").is_err());
        assert!(get_reg("{1}d").is_err());
        assert!(get_reg("a|*b").is_err());
        assert_eq!(
            get_reg("(a*)?").unwrap().get_pattern(),
            &Quantifier(
                Box::new(Group(Box::new(Quantifier(Box::new(Literal('a')), 0, None)))),
                0,
                Some(1)
            )
        );
    }

    #[test]
    fn regex_quantifier_invalid_syntax() {
        assert!(get_reg("a{").is_err());
        assert!(get_reg("a{1").is_err());
        assert!(get_reg("a{1,").is_err());
        assert!(get_reg("a{,1}").is_err());
        assert!(get_reg("a{1, }").is_err());
        assert!(get_reg("a{foo}").is_err());
        assert!(get_reg("a{1,2,3}").is_err());
        assert!(get_reg("a{").is_err());
    }

    #[test]
    fn regex_invalid_escape_sequences() {
        assert!(get_reg(r"\z").is_err());
        assert!(
            get_reg(r"\\z").unwrap().get_pattern()
                == &Concatenation(vec![Box::new(Literal('\\')), Box::new(Literal('z'))])
        );
        assert!(get_reg(r"\").is_err());
        assert!(get_reg(r"a\").is_err());
    }

    #[test]
    fn regex_range_unclosed() {
        assert!(get_reg("[a").is_err());
        assert!(get_reg("[a-").is_err());
        assert!(get_reg("[a-z").is_err());
        assert!(get_reg("[^a").is_err());
        assert!(get_reg("[\\").is_err());
    }

    #[test]
    fn regex_range_invalid_hyphen_placement() {
        let reg = get_reg(r"[a-z-]").unwrap();
        assert_eq!(
            reg.get_pattern(),
            &Range(vec![MultiChar('a', 'z'), SingleChar('-')])
        );
        let reg = get_reg(r"[-a-z]").unwrap();
        assert_eq!(
            reg.get_pattern(),
            &Range(vec![SingleChar('-'), MultiChar('a', 'z')])
        );
        assert!(get_reg(r"[a--z]").is_err());
    }

    #[test]
    fn regex_range_escaped_metacharacters() {
        let reg = get_reg(r"[\[-\]]").unwrap();
        assert_eq!(
            reg.get_pattern(),
            &Range(vec![SingleChar('['), SingleChar('-'), SingleChar(']')])
        );
        let reg = get_reg(r"[^\w]").unwrap();
        assert_eq!(
            reg.get_pattern(),
            &NotRange(vec![SingleEscape(WordCharacter)])
        );
    }

    #[test]
    fn regex_invalid_metacharacter_as_literal() {
        assert!(get_reg("+").is_err());
        assert!(get_reg("*").is_err());
        assert!(get_reg("?").is_err());
        assert!(get_reg("{").is_err());
        assert!(get_reg("}").is_err());
    }

    #[test]
    fn regex_empty_string() {
        assert!(get_reg("").is_err());
    }

    #[test]
    fn regex_just_anchors() {
        assert!(get_reg("^").unwrap().get_pattern() == &StartAnchor);
        assert!(get_reg("$").unwrap().get_pattern() == &EndAnchor);
        assert!(
            get_reg("^$").unwrap().get_pattern()
                == &Concatenation(vec![Box::new(StartAnchor), Box::new(EndAnchor)])
        );
    }

    #[test]
    fn regex_dollar_inside_group() {
        let reg = get_reg(r"(a$|b)").unwrap();
        let ans = Group(Box::new(Alternation(vec![
            Box::new(Concatenation(vec![
                Box::new(Literal('a')),
                Box::new(EndAnchor),
            ])),
            Box::new(Literal('b')),
        ])));
        assert_eq!(reg.get_pattern(), &ans);
    }
}
