use std::iter::Peekable;

#[derive(Clone, Debug)]
pub enum RegexPattern {
    Literal(char),
    Quantifier(Box<RegexPattern>, usize, Option<usize>),
    Not(Box<RegexPattern>),
    Group(Box<RegexPattern>),
    Range(Vec<RangeType>),
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

#[derive(Clone, Debug)]
pub enum RangeType {
    Range((char, char)),
    Char(char),
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
            Some('|') => {
                text.next();
            }
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
        Some('{') => parse_quantifier(text, atom)?,
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
            let Some(']') = text.next() else {
                return Err("Unclosed character class: Expected ']'".to_string());
            };
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
    loop {
        match text.peek() {
            Some('0'..='9') => {
                num_str.push(text.next().unwrap());
            }
            _ => break,
        }
    }
    if num_str.is_empty() {
        Err("Expected digits for quantifier".to_string())
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
                    ranges.push(RangeType::Range((start_range.unwrap(), x)));
                    match text.peek() {
                        Some('-') => {
                            return Err(
                                "Invalid start of range immediately after last range".to_string()
                            );
                        }
                        Some(_) => (),
                        None => return Err("Invalid end of range".to_string()),
                    }
                    start_range = None;
                } else {
                    if let Some(char) = last_char {
                        ranges.push(RangeType::Char(char));
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
    use super::*;

    #[test]
    fn simple() {
        let reg = Regex::new("abcd").unwrap();
        println!("{:?}", reg.get_pattern());
        assert!(false);
    }
}
