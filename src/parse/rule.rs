#[derive(Copy, Clone)]
pub enum Rule {
    Expression,
    BraceExpression,
    BraceStatements,
    Statement,
    IfElse,
    ForLoop,
    Equality,
    Comparison,
    PlusMinus,
    MultDiv,
    Unary,
    Primary,
    Terminal,
}
