#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    If,
    Else,
    For,
    While,
    Function,
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
    Return,
    Symbol(String),
    IntLiteral(i64),
    FloatLiteral(f64),
    StringLiteral(String),
}

#[derive(Clone, Debug)]
pub enum TokenizeErrorType {
    General,
    IntParse,
    FloatParse,
    StringParse,
}

#[derive(Clone, Debug)]
pub struct TokenizeError {
    pub line_number: usize,
    pub type_data: TokenizeErrorType,
}

pub struct Tokens {
    data: Vec<Token>,
}

impl Tokens {
    pub fn new() -> Self {
        Self { data: vec![] }
    }

    /// for use in the tokenizer. makes sure that developers can't add new
    /// tokens w/o considering the new index
    pub fn add_token(
        &mut self,
        index: &mut usize,
        token: Token,
        advance_by: usize,
    ) {
        self.data.push(token);
        *index += advance_by;
    }

    /// for use in the tokenizer. makes sure that developers can't add new
    /// tokens w/o considering the new index
    pub fn add_token_set_index(
        &mut self,
        index: &mut usize,
        token: Token,
        set_to: usize,
    ) {
        self.data.push(token);
        *index = set_to;
    }

    pub fn get(&self, index: usize) -> Option<&Token> {
        self.data.get(index)
    }

    pub fn len(&self) -> usize {
        self.data.len()
    }
}
