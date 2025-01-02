#[derive(Clone, Debug, Default)]
pub struct TypeError {
    pub start_line: usize,
    pub end_line: usize,
    pub info: String,
}
