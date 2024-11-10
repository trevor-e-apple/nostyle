#[derive(Clone, Debug, Default)]
pub enum ParseErrorType {
    #[default]
    NoError,
    General,
}

#[derive(Clone, Debug, Default)]
pub struct ParseError {
    pub line_number: usize,
    pub info: String,
    pub type_data: ParseErrorType,
}
