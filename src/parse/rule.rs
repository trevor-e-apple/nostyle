#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Rule {
    Expression,
    FunctionDef,
    FunctionDefParameters,
    ReturnsData,
    Declaration,
    BraceExpression,
    BraceStatements,
    Statement,
    ReturnStatement,
    IfElse,
    ForLoop,
    Equality,
    Comparison,
    PlusMinus,
    MultDiv,
    Unary,
    FunctionCall,
    FunctionArguments,
    Primary,
    Terminal,
    DeclarationStatements,
    DataStructure,
    StructAccess,
    StructAccessTerminal,
}
