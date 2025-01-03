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
    PlusEquals,
    MinusEquals,
    TimesEquals,
    DivideEquals,
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
    Struct,
    Returns,
    Symbol(String),
    IntLiteral(i64),
    FloatLiteral(f64),
    StringLiteral(String),
}

#[allow(dead_code)]
#[derive(Clone, Debug)]
pub enum TokenizeErrorType {
    General,
    Int,
    Float,
    String,
}

#[derive(Clone, Debug)]
pub struct TokenizeError {
    pub line_number: usize,
    pub type_data: TokenizeErrorType,
}

pub struct Tokens {
    data: Vec<Token>,
    line_numbers: Vec<usize>,
}

impl Tokens {
    pub fn new() -> Self {
        Self { data: vec![], line_numbers: vec![] }
    }

    /// for use in the tokenizer. makes sure that developers can't add new
    /// tokens w/o considering the new index
    pub fn add_token(
        &mut self,
        index: &mut usize,
        token: Token,
        line_number: usize,
        advance_by: usize,
    ) {
        self.data.push(token);
        self.line_numbers.push(line_number);
        *index += advance_by;
    }

    /// for use in the tokenizer. makes sure that developers can't add new
    /// tokens w/o considering the new index
    pub fn add_token_set_index(
        &mut self,
        index: &mut usize,
        token: Token,
        line_number: usize,
        set_to: usize,
    ) {
        self.data.push(token);
        self.line_numbers.push(line_number);
        *index = set_to;
    }

    pub fn get(&self, index: usize) -> Option<(Token, usize)> {
        match self.data.get(index) {
            Some(token) => {
                // token and line number are always added and removed together
                let line_number =
                    unsafe { *self.line_numbers.get_unchecked(index) };
                Some((token.clone(), line_number))
            }
            None => None,
        }
    }

    pub fn get_token(&self, index: usize) -> Option<&Token> {
        self.data.get(index)
    }

    pub fn get_line_number(&self, index: usize) -> Option<usize> {
        match self.line_numbers.get(index) {
            Some(line_number) => Some(*line_number),
            None => None,
        }
    }

    pub fn get_final_line(&self) -> usize {
        if self.line_numbers.len() == 0 {
            return 0;
        }

        unsafe { *self.line_numbers.get_unchecked(self.line_numbers.len() - 1) }
    }

    pub fn len(&self) -> usize {
        self.data.len()
    }

    pub fn token_slice(&self, start: usize, end: usize) -> &[Token] {
        &self.data[start..end]
    }
}
