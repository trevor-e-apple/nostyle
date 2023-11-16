use core::panic;
use std::{format, todo};

pub enum Token {
    If,
    Else,
    For,
    While,
    Function,
    Int8,
    Int16,
    Int32,
    Int64,
    UInt8,
    UInt16,
    UInt32,
    UInt64,
    Float32,
    Float64,
    Plus,
    Minus,
    Times,
    Divide,
    BoolEquals,
    NotEquals,
    LessThan,
    GreaterThan,
    LessThanOrEqual,
    GreaterThanOrEqual,
    Not,
    Assign,
    LParen,
    RParen,
    LBrace,
    RBrace,
    EndStatement,
    Comma,
    Dot,
    DoubleColon,
    Symbol(String),
    IntLiteral(i64),
    FloatLiteral(f64),
    StringLiteral(String),
}

pub struct InvalidTokenError {
    line_number: usize,
    token: String,
}

pub enum TokenizeError {
    InvalidToken(InvalidTokenError),
}

const ALPHABET_CHARS: [char; 52] = [
    'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o',
    'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', 'A', 'B', 'C', 'D',
    'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S',
    'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
];
const DIGIT_CHARS: [char; 10] =
    ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'];
const UNARY_OPERATORS: [char; 2] = ['!', '-'];

/// tokenize a string starting with an exclamation mark
fn tokenize_exclamation(
    result: &mut Vec<Token>,
    tokenize_errors: &mut Vec<TokenizeError>,
    token_string: &str,
    line_number: usize,
) {
    if token_string.len() == 2 {
        match token_string.chars().nth(1) {
            Some(value) => {
                if value == '=' {
                    result.push(Token::NotEquals);
                } else if ALPHABET_CHARS.contains(&value) {
                    tokenize_alphabet_char(result, &token_string[2..]);
                } else {
                    tokenize_errors.push(TokenizeError::InvalidToken(
                        InvalidTokenError {
                            line_number: line_number,
                            token: token_string.to_string(),
                        },
                    ));
                }
            }
            // None should never happen (token string is at least len 1)
            None => panic!(),
        }
    } else {
        let mut char_iter = token_string.chars().enumerate().peekable();
        // get all unary operators out of the way
        for (_, c) in char_iter {
            if c == '!' {
                result.push(Token::Not);
            } else {
                break;
            }
        }

        match char_iter.peek() {
            Some((next_char_index, next_char)) => {
                if ALPHABET_CHARS.contains(next_char) {
                    // followed by symbol
                    tokenize_alphabet_char(
                        result,
                        &token_string[*next_char_index..],
                    );
                } else {
                    // followed by non-symbol. error
                    tokenize_errors.push(TokenizeError::InvalidToken(
                        InvalidTokenError {
                            line_number: line_number,
                            token: token_string.to_string(),
                        },
                    ));
                }
            }
            None => {
                // unary operators followed by nothing is an error
                tokenize_errors.push(TokenizeError::InvalidToken(
                    InvalidTokenError {
                        line_number: line_number,
                        token: token_string.to_string(),
                    },
                ));
            }
        };
    }
}

/// checks whether or not the token_string begins with a character in the
/// alphabet
fn starts_with_alphabet_char(token_string: &str) -> bool {
    match token_string.chars().nth(0) {
        Some(value) => ALPHABET_CHARS.contains(&value),
        None => false,
    }
}

fn tokenize_alphabet_char(result: &mut Vec<Token>, token_string: &str) {
    result.push(Token::Symbol(token_string.to_string()));
}

/// tokenize the input data
pub fn tokenize(data: String) -> Result<Vec<Token>, Vec<TokenizeError>> {
    let mut result: Vec<Token> = vec![];
    let mut tokenize_errors: Vec<TokenizeError> = vec![];

    // track line number for errors
    let mut line_number: usize = 0;

    for token_string in data.split(" ") {
        if token_string == "if" {
            result.push(Token::If);
        } else if token_string == "else" {
            result.push(Token::Else);
        } else if token_string == "for" {
            result.push(Token::For);
        } else if token_string == "while" {
            result.push(Token::While);
        } else if token_string == "fn" {
            result.push(Token::Function);
        } else if token_string == "int8" {
            result.push(Token::Int8);
        } else if token_string == "int16" {
            result.push(Token::Int16);
        } else if token_string == "int32" {
            result.push(Token::Int32);
        } else if token_string == "int64" {
            result.push(Token::Int64);
        } else if token_string == "uint8" {
            result.push(Token::UInt8);
        } else if token_string == "uint16" {
            result.push(Token::UInt16);
        } else if token_string == "uint32" {
            result.push(Token::UInt32);
        } else if token_string == "uint64" {
            result.push(Token::UInt64);
        } else if token_string == "float32" {
            result.push(Token::Float32);
        } else if token_string == "float64" {
            result.push(Token::Float64);
        } else if token_string == "+" {
            result.push(Token::Plus);
        } else if token_string.starts_with("-") {
            result.push(Token::Minus);
        } else if token_string == "*" {
            result.push(Token::Times);
        } else if token_string == "/" {
            result.push(Token::Divide);
        } else if token_string.starts_with("=") {
            if token_string.len() == 1 {
                result.push(Token::Assign);
            } else if token_string.len() == 2 {
                let char_one = match token_string.chars().nth(1) {
                    Some(value) => value,
                    // None should never happen (token string is at least len 1)
                    None => panic!("Unexpected token length"),
                };

                if char_one == '=' && token_string.len() == 2 {
                    result.push(Token::BoolEquals);
                } else if UNARY_OPERATORS.contains(&char_one) {
                    if char_one == '!' {
                        tokenize_exclamation(
                            &mut result,
                            &mut tokenize_errors,
                            &token_string[2..],
                            line_number,
                        );
                    } else if char_one == '-' {
                        tokenize_minus(
                            &mut result,
                            &mut tokenize_errors,
                            &token_string[2..],
                            line_number,
                        );
                    } else {
                        panic!("Unexpected unary operator");
                    }
                }
            } else {
                tokenize_errors.push(TokenizeError::InvalidToken(
                    InvalidTokenError {
                        line_number: line_number,
                        token: token_string.to_string(),
                    },
                ));
            }
        } else if token_string.starts_with("!") {
            tokenize_exclamation(
                &mut result,
                &mut tokenize_errors,
                token_string,
                line_number,
            );
        } else if starts_with_alphabet_char(token_string) {
            tokenize_alphabet_char(&mut result, token_string);
        } else if token_string == "\n" {
            line_number += 1;
        }
    }

    if tokenize_errors.len() == 0 {
        Ok(result)
    } else {
        Err(tokenize_errors)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// test for assignment and equals (equivalence) working
    #[test]
    fn assign_vs_equals() {
        todo!();
    }

    /// test for a token with first two characters matching
    #[test]
    fn equals_trailing_data() {
        unimplemented!();
    }

    #[test]
    fn not_vs_not_equals() {
        unimplemented!();
    }

    #[test]
    fn basic_tokenize() {
        unimplemented!();
    }

    /// basic test for whether or not the line number is correct in a tokenize
    /// error
    #[test]
    fn error_line_number() {
        unimplemented!();
    }

    /// test multiple exclamation marks successively
    #[test]
    fn multiple_exclamation() {
        unimplemented!();
    }
}
