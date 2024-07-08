pub mod tokens;

use core::panic;
use std::{todo, vec};

use self::tokens::{Token, TokenizeError, TokenizeErrorType, Tokens};

const ALPHABET_CHARS: [char; 52] = [
    'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o',
    'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', 'A', 'B', 'C', 'D',
    'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S',
    'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
];
const FLOATING_POINT_DELIMITER: char = '.';
const DIGIT_CHARS: [char; 10] =
    ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'];
const HEX_CHARS: [char; 22] = [
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E',
    'F', 'a', 'b', 'c', 'd', 'e', 'f',
];
const BIN_CHARS: [char; 2] = ['0', '1'];

/// assuming that index points to the start of the word, returns the word string
fn get_word(chars: &Vec<char>, index: usize) -> String {
    let start = index;
    let mut index = index;
    while index < chars.len() {
        let c = match chars.get(index) {
            Some(c) => c,
            None => panic!(),
        };

        if ALPHABET_CHARS.contains(c) || DIGIT_CHARS.contains(c) || *c == '_' {
            index += 1;
        } else {
            break;
        }
    }

    chars[start..index].iter().map(|c| c.to_string()).collect()
}

/// TODO: document me
/// the characters that this function processes should not including the literal
/// prefix, e.g. 0b or 0x
fn get_int_literal_token(
    chars: &Vec<char>,
    index: usize,
    line_number: usize,
    base: u32,
    valid_chars: &[char],
) -> Result<(Token, usize), TokenizeError> {
    let start = index;
    let mut index = index;
    while index < chars.len() {
        let c = match chars.get(index) {
            Some(c) => c,
            None => panic!(),
        };

        if valid_chars.contains(c) {
            index += 1;
        } else {
            break;
        }
    }

    let num_literal_string: String =
        chars[start..index].iter().map(|c| c.to_string()).collect();

    match i64::from_str_radix(&num_literal_string, base) {
        Ok(value) => Ok((Token::IntLiteral(value), num_literal_string.len())),
        Err(_) => Err(TokenizeError {
            line_number,
            type_data: TokenizeErrorType::IntParse,
        }),
    }
}

/// returns an IntLiteral or FloatLiteral token. based on the digits from index
/// the last matching digit.
fn get_num_token(
    chars: &Vec<char>,
    index: usize,
    line_number: usize,
) -> Result<(Token, usize), TokenizeError> {
    let start = index;
    let mut is_float = false;
    let mut index = index;
    while index < chars.len() {
        let c = match chars.get(index) {
            Some(c) => c,
            None => panic!(),
        };

        if DIGIT_CHARS.contains(c) || *c == FLOATING_POINT_DELIMITER {
            if *c == FLOATING_POINT_DELIMITER {
                is_float = true;
            }
            index += 1;
        } else {
            break;
        }
    }

    let num_literal_string: String =
        chars[start..index].iter().map(|c| c.to_string()).collect();

    if is_float {
        match num_literal_string.parse::<f64>() {
            Ok(value) => {
                Ok((Token::FloatLiteral(value), num_literal_string.len()))
            }
            Err(_) => Err(TokenizeError {
                line_number,
                type_data: TokenizeErrorType::FloatParse,
            }),
        }
    } else {
        match num_literal_string.parse::<i64>() {
            Ok(value) => {
                Ok((Token::IntLiteral(value), num_literal_string.len()))
            }
            Err(_) => Err(TokenizeError {
                line_number,
                type_data: TokenizeErrorType::IntParse,
            }),
        }
    }
}

/// tokenize the input data
pub fn tokenize(data: &str) -> Result<Tokens, Vec<TokenizeError>> {
    let mut tokens = Tokens::new();
    let mut tokenize_errors: Vec<TokenizeError> = vec![];

    // track line number for errors
    let mut line_number: usize = 1;

    let chars: Vec<char> = data.chars().collect();
    let mut index = 0;
    while let Some(c) = chars.get(index) {
        if *c == 'i' {
            let word = get_word(&chars, index);
            if word == "if" {
                tokens.add_token(&mut index, Token::If, 2);
            } else {
                let word_len = word.len();
                tokens.add_token(&mut index, Token::Symbol(word), word_len);
            }
        } else if *c == 'e' {
            let word = get_word(&chars, index);
            if word == "else" {
                tokens.add_token(&mut index, Token::Else, 4);
            } else {
                let word_len = word.len();
                tokens.add_token(&mut index, Token::Symbol(word), word_len);
            }
        } else if *c == 'f' {
            let word = get_word(&chars, index);
            if word == "fn" {
                tokens.add_token(&mut index, Token::Function, 2);
            } else if word == "for" {
                tokens.add_token(&mut index, Token::For, 3);
            } else {
                let word_len = word.len();
                tokens.add_token(&mut index, Token::Symbol(word), word_len);
            }
        } else if *c == 'r' {
            let word = get_word(&chars, index);
            if word == "return" {
                tokens.add_token(&mut index, Token::Return, 6);
            } else {
                let word_len = word.len();
                tokens.add_token(&mut index, Token::Symbol(word), word_len);
            }
        } else if *c == 'w' {
            let word = get_word(&chars, index);
            if word == "while" {
                tokens.add_token(&mut index, Token::While, 5);
            } else {
                let word_len = word.len();
                tokens.add_token(&mut index, Token::Symbol(word), word_len);
            }
        } else if *c == '+' {
            tokens.add_token(&mut index, Token::Plus, 1);
        } else if *c == '-' {
            tokens.add_token(&mut index, Token::Minus, 1);
        } else if *c == '*' {
            tokens.add_token(&mut index, Token::Times, 1);
        } else if *c == '/' {
            tokens.add_token(&mut index, Token::Divide, 1);
        } else if *c == '=' {
            match chars.get(index + 1) {
                Some(next_char) => {
                    if *next_char == '=' {
                        tokens.add_token(&mut index, Token::BoolEquals, 2);
                    } else {
                        tokens.add_token(&mut index, Token::Assign, 1);
                    }
                }
                None => tokens.add_token(&mut index, Token::Assign, 1),
            }
        } else if *c == '!' {
            match chars.get(index + 1) {
                Some(next_char) => {
                    if *next_char == '=' {
                        tokens.add_token(&mut index, Token::NotEquals, 2);
                    } else {
                        tokens.add_token(&mut index, Token::Not, 1);
                    }
                }
                None => tokens.add_token(&mut index, Token::Not, 1),
            }
        } else if *c == '{' {
            tokens.add_token(&mut index, Token::LBrace, 1);
        } else if *c == '}' {
            tokens.add_token(&mut index, Token::RBrace, 1);
        } else if *c == '(' {
            tokens.add_token(&mut index, Token::LParen, 1);
        } else if *c == ')' {
            tokens.add_token(&mut index, Token::RParen, 1);
        } else if ALPHABET_CHARS.contains(c) {
            let word = get_word(&chars, index);
            let word_len = word.len();
            tokens.add_token(&mut index, Token::Symbol(word), word_len);
        } else if DIGIT_CHARS.contains(c) {
            if *c == '0' {
                match chars.get(index + 1) {
                    Some(next_char) => {
                        if *next_char == 'x' || *next_char == 'b' {
                            // the int literal cases
                            let (starts_from, base, valid_chars) =
                                if *next_char == 'x' {
                                    (index + 2, 16, &HEX_CHARS[0..])
                                } else if *next_char == 'b' {
                                    (index + 2, 2, &BIN_CHARS[0..])
                                } else {
                                    panic!("Unhandled literal prefix");
                                };

                            match get_int_literal_token(
                                &chars,
                                starts_from,
                                line_number,
                                base,
                                valid_chars,
                            ) {
                                Ok((token, string_len)) => tokens.add_token(
                                    &mut index,
                                    token,
                                    string_len + 2, // + 2 for the literal prefix indicating the base
                                ),
                                Err(error) => {
                                    tokenize_errors.push(error);
                                    index += 1;
                                }
                            };
                        } else {
                            match get_num_token(&chars, index, line_number) {
                                Ok((token, string_len)) => {
                                    tokens.add_token(
                                        &mut index, token, string_len,
                                    );
                                }
                                Err(_) => todo!(),
                            }
                        }
                    }
                    None => {
                        tokens.add_token(&mut index, Token::IntLiteral(0), 1)
                    }
                }
            } else {
                match get_num_token(&chars, index, line_number) {
                    Ok((token, string_len)) => {
                        tokens.add_token(&mut index, token, string_len)
                    }
                    Err(err) => {
                        tokenize_errors.push(err);
                        index += 1;
                    }
                }
            }
        } else if *c == '"' {
            let start = index + 1;
            let mut end = start;
            let mut has_terminal = false;
            while end < chars.len() {
                let next_char = match chars.get(end) {
                    Some(next_char) => next_char,
                    None => {
                        break;
                    }
                };

                if *next_char != '"' {
                    if *next_char == '\n' {
                        line_number += 1;
                    }
                    end += 1;
                } else {
                    has_terminal = true;
                    break;
                }
            }

            if has_terminal {
                let string: String =
                    chars[start..end].iter().map(|c| c.to_string()).collect();
                // index = end + 1 to move past quotation mark
                tokens.add_token_set_index(
                    &mut index,
                    Token::StringLiteral(string),
                    end + 1,
                );
            } else {
                tokenize_errors.push(TokenizeError {
                    line_number,
                    type_data: TokenizeErrorType::StringParse,
                });
                index = end;
            }
        } else if *c == ';' {
            tokens.add_token(&mut index, Token::EndStatement, 1);
        } else if *c == ',' {
            tokens.add_token(&mut index, Token::Comma, 1);
        } else if *c == '<' {
            match chars.get(index + 1) {
                Some(next_char) => {
                    if *next_char == '=' {
                        tokens.add_token(&mut index, Token::LessThanOrEqual, 2);
                    } else {
                        tokens.add_token(&mut index, Token::LessThan, 1);
                    }
                }
                None => {
                    tokens.add_token(&mut index, Token::LessThan, 1);
                }
            };
        } else if *c == '>' {
            match chars.get(index + 1) {
                Some(next_char) => {
                    if *next_char == '=' {
                        tokens.add_token(
                            &mut index,
                            Token::GreaterThanOrEqual,
                            2,
                        );
                    } else {
                        tokens.add_token(&mut index, Token::GreaterThan, 1);
                    }
                }
                None => {
                    tokens.add_token(&mut index, Token::GreaterThan, 1);
                }
            };
        } else if *c == '.' {
            tokens.add_token(&mut index, Token::Dot, 1);
        } else if *c == '\n' {
            line_number += 1;
            index += 1;
        } else if *c == ' ' {
            index += 1;
        } else {
            todo!("string literals");
        }
    }

    if tokenize_errors.is_empty() {
        Ok(tokens)
    } else {
        Err(tokenize_errors)
    }
}

#[cfg(test)]
mod tests {
    use std::assert_eq;

    use super::*;

    /// test a couple of basic tokens being parsed correctly
    #[test]
    fn basic_tokenize() {
        let s = "int32 a = b + c;";
        let tokens = tokenize(&s).expect("Unexpected tokenize error");

        assert_eq!(tokens.len(), 7);
        assert_eq!(*tokens.get(0).unwrap(), Token::Symbol("int32".to_owned()));
        assert_eq!(*tokens.get(1).unwrap(), Token::Symbol("a".to_owned()));
        assert_eq!(*tokens.get(2).unwrap(), Token::Assign);
        assert_eq!(*tokens.get(3).unwrap(), Token::Symbol("b".to_owned()));
        assert_eq!(*tokens.get(4).unwrap(), Token::Plus);
        assert_eq!(*tokens.get(5).unwrap(), Token::Symbol("c".to_owned()));
        assert_eq!(*tokens.get(6).unwrap(), Token::EndStatement);
    }

    /// test that tokenizing across multiple lines is handled correctly
    #[test]
    fn multi_line_tokenization() {
        let s = "int32 a = b + c;\nint32 d = a;";
        let tokens = tokenize(&s).expect("Unexpected tokenize error");

        assert_eq!(tokens.len(), 12);
        assert_eq!(*tokens.get(0).unwrap(), Token::Symbol("int32".to_owned()));
        assert_eq!(*tokens.get(1).unwrap(), Token::Symbol("a".to_owned()));
        assert_eq!(*tokens.get(2).unwrap(), Token::Assign);
        assert_eq!(*tokens.get(3).unwrap(), Token::Symbol("b".to_owned()));
        assert_eq!(*tokens.get(4).unwrap(), Token::Plus);
        assert_eq!(*tokens.get(5).unwrap(), Token::Symbol("c".to_owned()));
        assert_eq!(*tokens.get(6).unwrap(), Token::EndStatement);
        assert_eq!(*tokens.get(7).unwrap(), Token::Symbol("int32".to_owned()));
        assert_eq!(*tokens.get(8).unwrap(), Token::Symbol("d".to_owned()));
        assert_eq!(*tokens.get(9).unwrap(), Token::Assign);
        assert_eq!(*tokens.get(10).unwrap(), Token::Symbol("a".to_owned()));
        assert_eq!(*tokens.get(11).unwrap(), Token::EndStatement);
    }

    /// test for assignment and equals (equivalence) working
    #[test]
    fn assign_vs_equals() {
        let s = "a = b; a == b;";
        let tokens = tokenize(&s).expect("Unexpected tokenize error");

        assert_eq!(tokens.len(), 8);
        assert_eq!(*tokens.get(0).unwrap(), Token::Symbol("a".to_string()));
        assert_eq!(*tokens.get(1).unwrap(), Token::Assign);
        assert_eq!(*tokens.get(2).unwrap(), Token::Symbol("b".to_string()));
        assert_eq!(*tokens.get(3).unwrap(), Token::EndStatement);
        assert_eq!(*tokens.get(4).unwrap(), Token::Symbol("a".to_string()));
        assert_eq!(*tokens.get(5).unwrap(), Token::BoolEquals);
        assert_eq!(*tokens.get(6).unwrap(), Token::Symbol("b".to_string()));
        assert_eq!(*tokens.get(7).unwrap(), Token::EndStatement);
    }

    /// tests for ! and != being tokenized correctly
    #[test]
    fn not_vs_not_equals() {
        let s = "!a != b;";
        let tokens = tokenize(&s).expect("Unexpected tokenize error");

        assert_eq!(tokens.len(), 5);
        assert_eq!(*tokens.get(0).unwrap(), Token::Not);
        assert_eq!(*tokens.get(1).unwrap(), Token::Symbol("a".to_string()));
        assert_eq!(*tokens.get(2).unwrap(), Token::NotEquals);
        assert_eq!(*tokens.get(3).unwrap(), Token::Symbol("b".to_string()));
        assert_eq!(*tokens.get(4).unwrap(), Token::EndStatement);
    }

    /// test for sequential unary operators being tokenized correctly
    #[test]
    fn sequential_minus() {
        let s = "---a;";
        let tokens = tokenize(&s).expect("Unexpected tokenize error");

        assert_eq!(tokens.len(), 5);
        assert_eq!(*tokens.get(0).unwrap(), Token::Minus);
        assert_eq!(*tokens.get(1).unwrap(), Token::Minus);
        assert_eq!(*tokens.get(2).unwrap(), Token::Minus);
        assert_eq!(*tokens.get(3).unwrap(), Token::Symbol("a".to_string()));
        assert_eq!(*tokens.get(4).unwrap(), Token::EndStatement);
    }

    /// test for tokenizing symbols and operators without spaces
    #[test]
    fn no_space() {
        let s = "a+b";
        let tokens = tokenize(&s).expect("Unexpected tokenize error");

        assert_eq!(tokens.len(), 3);
        assert_eq!(*tokens.get(0).unwrap(), Token::Symbol("a".to_string()));
        assert_eq!(*tokens.get(1).unwrap(), Token::Plus);
        assert_eq!(*tokens.get(2).unwrap(), Token::Symbol("b".to_string()));
    }

    /// basic test for whether or not the line number is correct in a tokenize
    /// error
    #[test]
    fn error_line_number() {
        let s = "a = b;\n\";";
        let errors = match tokenize(&s) {
            Ok(_) => {
                assert!(false);
                return;
            }
            Err(errors) => errors,
        };

        assert_eq!(errors.len(), 1);
        let error = errors[0].clone();
        assert_eq!(error.line_number, 2);
        match error.type_data {
            TokenizeErrorType::General => {
                assert!(false);
                return;
            }
            TokenizeErrorType::IntParse => {
                assert!(false);
                return;
            }
            TokenizeErrorType::FloatParse => {
                assert!(false);
                return;
            }
            TokenizeErrorType::StringParse => {}
        }
    }

    /// test multiple exclamation marks successively
    #[test]
    fn multiple_exclamation() {
        let s = "!!!a != b;";
        let tokens = tokenize(&s).expect("Unexpected tokenize error");

        assert_eq!(tokens.len(), 7);
        assert_eq!(*tokens.get(0).unwrap(), Token::Not);
        assert_eq!(*tokens.get(1).unwrap(), Token::Not);
        assert_eq!(*tokens.get(2).unwrap(), Token::Not);
        assert_eq!(*tokens.get(3).unwrap(), Token::Symbol("a".to_string()));
        assert_eq!(*tokens.get(4).unwrap(), Token::NotEquals);
        assert_eq!(*tokens.get(5).unwrap(), Token::Symbol("b".to_string()));
        assert_eq!(*tokens.get(6).unwrap(), Token::EndStatement);
    }

    /// 'fo' is tokenized as a symbol, not a 'for'
    #[test]
    fn fo_only() {
        let s = "fo";
        let tokens = tokenize(&s).expect("Unexpected tokenize error");

        assert_eq!(tokens.len(), 1);
        assert_eq!(*tokens.get(0).unwrap(), Token::Symbol("fo".to_string()));
    }

    /// 'for' is tokenized as a 'for', not a symbol
    #[test]
    fn for_keyword() {
        let s = "for";
        let tokens = tokenize(&s).expect("Unexpected tokenize error");

        assert_eq!(tokens.len(), 1);
        assert_eq!(*tokens.get(0).unwrap(), Token::For);
    }

    /// 'forr' is tokenized as a symbol
    #[test]
    fn forr() {
        let s = "forr";
        let tokens = tokenize(&s).expect("Unexpected tokenize error");

        assert_eq!(tokens.len(), 1);
        assert_eq!(*tokens.get(0).unwrap(), Token::Symbol("forr".to_string()));
    }

    /// fn is tokenized as a Function token
    #[test]
    fn my_fn() {
        let s = "fn";
        let tokens = tokenize(&s).expect("Unexpected tokenize error");

        assert_eq!(tokens.len(), 1);
        assert_eq!(*tokens.get(0).unwrap(), Token::Function);
    }

    /// fna is tokenized as a symbol token
    #[test]
    fn fna() {
        let s = "fna";
        let tokens = tokenize(&s).expect("Unexpected tokenize error");

        assert_eq!(tokens.len(), 1);
        assert_eq!(*tokens.get(0).unwrap(), Token::Symbol("fna".to_owned()));
    }

    /// test int literal
    #[test]
    fn int_literal() {
        let s = "11 + 23";
        let tokens = tokenize(&s).expect("Unexpected tokenize error");

        assert_eq!(tokens.len(), 3);
        assert_eq!(*tokens.get(0).unwrap(), Token::IntLiteral(11));
        assert_eq!(*tokens.get(1).unwrap(), Token::Plus);
        assert_eq!(*tokens.get(2).unwrap(), Token::IntLiteral(23));
    }

    /// test float literal
    #[test]
    fn float_literal() {
        let s = "1.2 + 3.45 + 6.789";
        let tokens = tokenize(&s).expect("Unexpected tokenize error");

        assert_eq!(tokens.len(), 5);
        assert_eq!(*tokens.get(0).unwrap(), Token::FloatLiteral(1.2));
        assert_eq!(*tokens.get(1).unwrap(), Token::Plus);
        assert_eq!(*tokens.get(2).unwrap(), Token::FloatLiteral(3.45));
        assert_eq!(*tokens.get(3).unwrap(), Token::Plus);
        assert_eq!(*tokens.get(4).unwrap(), Token::FloatLiteral(6.789));
    }

    /// test negative numeric literal
    #[test]
    fn negative_num_literal() {
        let s = "-1";
        let tokens = tokenize(&s).expect("Unexpected tokenize error");

        assert_eq!(tokens.len(), 2);
        assert_eq!(*tokens.get(0).unwrap(), Token::Minus);
        assert_eq!(*tokens.get(1).unwrap(), Token::IntLiteral(1));
    }

    /// test for hex literal with lower case
    #[test]
    fn hex_literal() {
        let s = "0xabcdef";
        let tokens = tokenize(&s).expect("Unexpected tokenize error");

        assert_eq!(tokens.len(), 1);
        assert_eq!(*tokens.get(0).unwrap(), Token::IntLiteral(0xabcdef));
    }

    /// test for hex literal with caps
    #[test]
    fn hex_literal_caps() {
        let s = "0xABCDEF";
        let tokens = tokenize(&s).expect("Unexpected tokenize error");

        assert_eq!(tokens.len(), 1);
        assert_eq!(*tokens.get(0).unwrap(), Token::IntLiteral(0xabcdef));
    }

    /// test for a binary literal
    #[test]
    fn binary_literal() {
        let s = "0b0101";
        let tokens = tokenize(&s).expect("Unexpected tokenize error");

        assert_eq!(tokens.len(), 1);
        assert_eq!(*tokens.get(0).unwrap(), Token::IntLiteral(0b0101));
    }

    /// test string literal
    #[test]
    fn string_literal() {
        let s = "\"my string\"";
        let tokens = tokenize(&s).expect("Unexpected tokenize error");
        assert_eq!(tokens.len(), 1);
        assert_eq!(
            *tokens.get(0).unwrap(),
            Token::StringLiteral("my string".to_string())
        );
    }

    /// test empty string literal
    #[test]
    fn empty_string_literal() {
        let s = "\"\"";
        let tokens = tokenize(&s).expect("Unexpected tokenize error");
        assert_eq!(tokens.len(), 1);
        assert_eq!(
            *tokens.get(0).unwrap(),
            Token::StringLiteral("".to_string())
        );
    }

    /// test multi line string literals
    #[test]
    fn multi_line_string_literals() {
        let s = "\"a\nb\"";
        let tokens = tokenize(&s).expect("Unexpected tokenize error");
        assert_eq!(tokens.len(), 1);
        assert_eq!(
            *tokens.get(0).unwrap(),
            Token::StringLiteral("a\nb".to_string())
        );
    }

    /// test for tokenizing zero in braces
    #[test]
    fn tokenize_zero_in_braces() {
        let s = "{0}";
        let tokens = tokenize(&s).expect("Unexpected tokenize error");
        assert_eq!(tokens.len(), 3);
        assert_eq!(*tokens.get(0).unwrap(), Token::LBrace);
        assert_eq!(*tokens.get(1).unwrap(), Token::IntLiteral(0));
        assert_eq!(*tokens.get(2).unwrap(), Token::RBrace);
    }

    #[test]
    fn tokenize_sudden_parens_in_symbol() {
        let s = "ab()d";
        let tokens = tokenize(&s).expect("Unexpected tokenize error");
        assert_eq!(tokens.len(), 4);
        assert_eq!(*tokens.get(0).unwrap(), Token::Symbol("ab".to_owned()));
        assert_eq!(*tokens.get(1).unwrap(), Token::LParen);
        assert_eq!(*tokens.get(2).unwrap(), Token::RParen);
        assert_eq!(*tokens.get(3).unwrap(), Token::Symbol("d".to_owned()));
    }

    #[test]
    fn tokenize_sudden_lparen_in_symbol() {
        let s = "ab(d";
        let tokens = tokenize(&s).expect("Unexpected tokenize error");
        assert_eq!(tokens.len(), 3);
        assert_eq!(*tokens.get(0).unwrap(), Token::Symbol("ab".to_owned()));
        assert_eq!(*tokens.get(1).unwrap(), Token::LParen);
        assert_eq!(*tokens.get(2).unwrap(), Token::Symbol("d".to_owned()));
    }

    #[test]
    fn tokenize_rparen_in_symbol() {
        let s = "ab)d";
        let tokens = tokenize(&s).expect("Unexpected tokenize error");
        assert_eq!(tokens.len(), 3);
        assert_eq!(*tokens.get(0).unwrap(), Token::Symbol("ab".to_owned()));
        assert_eq!(*tokens.get(1).unwrap(), Token::RParen);
        assert_eq!(*tokens.get(2).unwrap(), Token::Symbol("d".to_owned()));
    }

    #[test]
    fn tokenize_function_call() {
        let s = "ab(d)";
        let tokens = tokenize(&s).expect("Unexpected tokenize error");
        assert_eq!(tokens.len(), 4);
        assert_eq!(*tokens.get(0).unwrap(), Token::Symbol("ab".to_owned()));
        assert_eq!(*tokens.get(1).unwrap(), Token::LParen);
        assert_eq!(*tokens.get(2).unwrap(), Token::Symbol("d".to_owned()));
        assert_eq!(*tokens.get(3).unwrap(), Token::RParen);
    }

    #[test]
    fn tokenize_underscore() {
        let s = "a_b";
        let tokens = tokenize(&s).expect("Unexpected tokenize error");
        assert_eq!(tokens.len(), 1);
        assert_eq!(*tokens.get(0).unwrap(), Token::Symbol("a_b".to_owned()));
    }

    #[test]
    fn tokenize_return() {
        let s = "return a;";
        let tokens = tokenize(&s).expect("Unexpected tokenize error");
        assert_eq!(tokens.len(), 3);
        assert_eq!(*tokens.get(0).unwrap(), Token::Return);
        assert_eq!(*tokens.get(1).unwrap(), Token::Symbol("a".to_owned()));
        assert_eq!(*tokens.get(2).unwrap(), Token::EndStatement);
    }

    #[test]
    fn tokenize_r_start() {
        let s = "return r_value;";
        let tokens = tokenize(&s).expect("Unexpected tokenize error");
        assert_eq!(tokens.len(), 1);
        assert_eq!(*tokens.get(0).unwrap(), Token::Return);
        assert_eq!(
            *tokens.get(1).unwrap(),
            Token::Symbol("r_value".to_owned())
        );
        assert_eq!(*tokens.get(2).unwrap(), Token::EndStatement);
    }
}
