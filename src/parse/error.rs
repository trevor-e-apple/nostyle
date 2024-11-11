#[derive(Clone, Debug, Default)]
pub struct ParseError {
    pub line_number: usize,
    pub info: String,
}
