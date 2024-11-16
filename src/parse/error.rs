#[derive(Clone, Debug, Default)]
pub struct ParseError {
    pub start_line: usize,
    pub end_line: usize,
    pub info: String,
}
