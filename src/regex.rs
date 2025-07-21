use std::iter::Peekable;

#[derive(Clone, Debug, PartialEq)]
pub enum RegexPattern {
    Literal(char),
    Quantifier(Box<RegexPattern>, usize, Option<usize>),
    Not(Box<RegexPattern>), //TODO
    Group(Box<RegexPattern>),
    Range(Vec<RangeType>), //Needs checks for if range provided is value (ex: 8-a is not valid)
    Concatenation(Vec<Box<RegexPattern>>),
    Alternation(Vec<Box<RegexPattern>>),
    StartAnchor,
    EndAnchor,
    WordBoundry,
    AnyCharacter,
    Digits,
    Whitespace,
    WordCharacters,
}

#[derive(Clone, Debug, PartialEq)]
pub enum RangeType {
    Multi(char, char),
    Single(char),
}

pub struct Regex {
    pattern: RegexPattern,
}

impl Regex {
    pub fn new(text: &str) -> Result<Self, String> {
        let mut text_iter = text.chars().into_iter().peekable();
        let pattern = parse_expression(&mut text_iter)?;
        Ok(Regex { pattern })
    }

    pub fn get_pattern(&self) -> &RegexPattern {
        &self.pattern
    }
}

fn parse_expression<I>(text: &mut Peekable<I>) -> Result<RegexPattern, String>
where
    I: Iterator<Item = char>,
{
    let mut terms = Vec::new();
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
        return Err("Empty expression found, or unexpected end of input".to_string());
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
            let range_content = parse_range(text)?;
            Ok(RegexPattern::Range(range_content))
        }
        Some('^') => {
            text.next();
            Ok(RegexPattern::StartAnchor)
        }
        Some('$') => {
            text.next();
            Ok(RegexPattern::EndAnchor)
        }
        Some('.') => {
            text.next();
            Ok(RegexPattern::AnyCharacter)
        }
        Some('\\') => {
            text.next();
            let escaped_char = text
                .next()
                .ok_or_else(|| "Nothing followed \\".to_string())?;
            match escaped_char {
                'b' => Ok(RegexPattern::WordBoundry),
                'd' => Ok(RegexPattern::Digits),
                's' => Ok(RegexPattern::Whitespace),
                'w' => Ok(RegexPattern::WordCharacters),
                'n' => Ok(RegexPattern::Literal('\n')),
                '*' | '?' | '\\' | '(' | ')' | '[' | ']' | '{' | '}' | '^' | '$' | '|' | '.' => {
                    Ok(RegexPattern::Literal(escaped_char))
                }
                '\'' => Ok(RegexPattern::Literal('\'')),
                '"' => Ok(RegexPattern::Literal('"')),
                x => Err(format!("Invalid escape sequence: \\{x}")),
            }
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
                    return Err("Invalid end of range after -".to_string());
                }
                break;
            }
            Some('-') => {
                if start_range != None {
                    return Err("Invalid --".to_string());
                }
                start_range = last_char;
                continue;
            }
            Some(x) => {
                if start_range != None {
                    ranges.push(RangeType::Multi(start_range.unwrap(), x));
                    match text.peek() {
                        Some('-') => {
                            return Err("Invalid start of range immediately after last range (--)"
                                .to_string());
                        }
                        Some(_) => (),
                        None => (),
                    }
                    start_range = None;
                    last_char = Some(x);
                } else {
                    match text.peek() {
                        Some('-') => (),
                        Some(_) => ranges.push(RangeType::Single(x)),
                        None => (),
                    }
                    last_char = Some(x);
                }
            }
            None => return Err("Invalid end of range".to_string()),
        }
    }
    Ok(ranges)
}

#[cfg(test)]
mod tests {
    use super::{RangeType::*, RegexPattern::*, *};

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
            Box::new(WordCharacters),
            Box::new(Whitespace),
            Box::new(Digits),
            Box::new(WordBoundry),
        ]);
        assert_eq!(reg.get_pattern(), &ans);
    }

    #[test]
    fn regex_range_simple() {
        let reg = get_reg(r"[a-z]").unwrap();
        let ans = Range(vec![Multi('a', 'z')]);
        assert_eq!(reg.get_pattern(), &ans);
    }

    #[test]
    fn regex_range_multiple_ranges() {
        let reg = get_reg(r"[a-zA-Z0-9]").unwrap();
        let ans = Range(vec![Multi('a', 'z'), Multi('A', 'Z'), Multi('0', '9')]);
        assert_eq!(reg.get_pattern(), &ans);
    }

    #[test]
    fn regex_range_single_value() {
        let reg = get_reg(r"[a-zA-Z0-9_]").unwrap();
        let ans = Range(vec![
            Multi('a', 'z'),
            Multi('A', 'Z'),
            Multi('0', '9'),
            Single('_'),
        ]);
        assert_eq!(reg.get_pattern(), &ans);
    }

    #[test]
    fn regex_complex_varity() {
        let reg = get_reg(r"^[a-z]+|(01|\d.\s\w\b)$").unwrap();
        let ans = Alternation(vec![
            Box::new(Concatenation(vec![
                Box::new(StartAnchor),
                Box::new(Quantifier(Box::new(Range(vec![Multi('a', 'z')])), 1, None)),
            ])),
            Box::new(Concatenation(vec![
                Box::new(Group(Box::new(Alternation(vec![
                    Box::new(Concatenation(vec![
                        Box::new(Literal('0')),
                        Box::new(Literal('1')),
                    ])),
                    Box::new(Concatenation(vec![
                        Box::new(Digits),
                        Box::new(AnyCharacter),
                        Box::new(Whitespace),
                        Box::new(WordCharacters),
                        Box::new(WordBoundry),
                    ])),
                ])))),
                Box::new(EndAnchor),
            ])),
        ]);
        assert_eq!(reg.get_pattern(), &ans);
    }
}
