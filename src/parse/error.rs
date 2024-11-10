#[derive(Clone, Debug)]
pub enum ParseErrorType {
    NoError,
    General,
}

#[derive(Clone, Debug)]
pub struct ParseError {
    pub line_number: usize,
    pub info: String,
    pub type_data: ParseErrorType,
}
