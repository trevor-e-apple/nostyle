use std::fs::File;
use std::path::Path;
use std::println;
use std::time::SystemTime;

use crate::parse::ast::get_diff_string;
use crate::parse::*;

#[cfg(test)]
use crate::tokenize::tokenize;

/// helper function for adding an expression with nothing but a terminal
/// to an ast
fn add_terminal_expression(
    ast: &mut Ast,
    parent_handle: AstNodeHandle,
    terminal_value: Option<Token>,
    child_start: usize,
    len: usize,
) -> AstNodeHandle {
    let expression_handle =
        ast.add_child(parent_handle, Rule::Expression, child_start, len);
    ast.add_terminal_child(expression_handle, terminal_value, child_start, len);

    return expression_handle;
}

/// helper function for adding a child that just adds two tokens. Adds from
/// the "equality" rule downward
fn add_expected_add_child(
    ast: &mut Ast,
    parent_handle: AstNodeHandle,
    lhs_terminal: Token,
    lhs_start: usize,
    rhs_terminal: Token,
) {
    let plus_minus_handle = ast.add_child_with_data(
        parent_handle,
        Rule::PlusMinus,
        Some(Token::Plus),
        lhs_start,
        3,
    );

    // lhs (recursive)
    ast.add_terminal_child(plus_minus_handle, Some(lhs_terminal), lhs_start, 1);

    // rhs
    ast.add_terminal_child(
        plus_minus_handle,
        Some(rhs_terminal),
        lhs_start + 2,
        1,
    );
}

/// helper function for adding a child that just multiplies two terminal tokens. Adds
/// from the "equality" rule downward
fn add_expected_mult_child(
    ast: &mut Ast,
    parent_handle: AstNodeHandle,
    lhs_terminal: Token,
    lhs_start: usize,
    rhs_terminal: Token,
) {
    let mult_div_handle = ast.add_child_with_data(
        parent_handle,
        Rule::MultDiv,
        Some(Token::Times),
        lhs_start,
        3,
    );

    // lhs (recursive)
    ast.add_terminal_child(mult_div_handle, Some(lhs_terminal), lhs_start, 1);

    // rhs
    ast.add_terminal_child(
        mult_div_handle,
        Some(rhs_terminal),
        lhs_start + 2,
        1,
    );
}

/// a helper function for adding the ast nodes for a simple assignment from
/// one terminal to another
fn add_assignment_statement(
    ast: &mut Ast,
    parent_handle: AstNodeHandle,
    lhs_terminal: Token,
    lhs_start: usize,
    rhs_terminal: Token,
) {
    let statement_handle =
        ast.add_child(parent_handle, Rule::Statement, lhs_start, 4);

    // LHS
    let lhs_expression =
        ast.add_child(statement_handle, Rule::Expression, lhs_start, 1);
    ast.add_terminal_child(lhs_expression, Some(lhs_terminal), lhs_start, 1);

    // RHS
    add_terminal_expression(
        ast,
        statement_handle,
        Some(rhs_terminal),
        lhs_start + 2,
        1,
    );
}

/// adds no statements descendents to parent
fn add_no_statements(
    ast: &mut Ast,
    parent_handle: AstNodeHandle,
    start: usize,
    end: usize,
) {
    let brace_statements_handle =
        ast.add_child(parent_handle, Rule::BraceStatements, start, end);
    let statement_handle =
        ast.add_child(brace_statements_handle, Rule::Statement, start, end);
    add_terminal_expression(ast, statement_handle, None, start, 1);
}

// adds tree for basic binary comparison to parent expression
fn add_comparison_tree(
    ast: &mut Ast,
    parent_handle: AstNodeHandle,
    a: Token,
    a_start: usize,
    b: Token,
) {
    let equality_handle = ast.add_child_with_data(
        parent_handle,
        Rule::Equality,
        Some(Token::BoolEquals),
        a_start,
        3,
    );

    // a
    ast.add_terminal_child(equality_handle, Some(a), a_start, 1);

    // b
    ast.add_terminal_child(equality_handle, Some(b), a_start + 2, 1);
}

fn write_ast_dot(ast: &Ast, ast_name: &str, seconds_since: u64) {
    let file_name = {
        let mut file_name = format!("{}_{}.dot", ast_name, seconds_since);
        let mut attempt = 0;
        while Path::new(&file_name).exists() {
            file_name =
                format!("{}_{}_{}.dot", ast_name, seconds_since, attempt);
            attempt += 1;
        }
        file_name
    };
    let mut file = File::create(&file_name).expect("Unable to create dot file");
    match dot::render(ast, &mut file) {
        Ok(_) => println!("Ast dot file name: {}", file_name),
        Err(_) => println!("Error creating ast dot file"),
    };
}

/// helper function for tests to compare two asts and print out some info
/// if they don't match
fn check_ast_equal(ast: &Ast, expected_ast: &Ast) {
    println!("ast:");
    ast.print();
    println!("expected_ast:");
    expected_ast.print();
    println!("diff_ast:");
    print!("{}", get_diff_string(&ast, &expected_ast));

    let equivalent = Ast::equivalent(ast, expected_ast);
    if !equivalent {
        // write out dot file for comparing asts

        let now = SystemTime::now();
        let seconds_since = now
            .duration_since(SystemTime::UNIX_EPOCH)
            .expect("Unexpected failure to get system time")
            .as_secs();

        write_ast_dot(ast, "ast", seconds_since);
        write_ast_dot(expected_ast, "expected_ast", seconds_since);
    }
    assert!(equivalent);
}

/// test empty parse
#[test]
fn empty_parse() {
    let tokens = tokenize("").expect("Unexpected tokenize error");
    let ast = parse(&tokens).expect("Unexpected parse error");
    let expected_ast = Ast::new();

    check_ast_equal(&ast, &expected_ast);
}

#[test]
fn empty_braces() {
    let tokens = tokenize("{}").expect("Unexpected tokenize error");
    let ast = parse(&tokens).expect("Unexpected parse error");
    let expected_ast = {
        let mut expected_ast = Ast::new();
        let root_handle = expected_ast.add_root(Rule::Expression, 0, 2);
        let brace_expression_handle =
            expected_ast.add_child(root_handle, Rule::BraceExpression, 0, 2);
        let expression_handle = expected_ast.add_child(
            brace_expression_handle,
            Rule::Expression,
            1,
            0,
        );
        expected_ast.add_terminal_child(expression_handle, None, 1, 0);
        expected_ast
    };

    check_ast_equal(&ast, &expected_ast);
}

#[test]
fn empty_parens() {
    let tokens = tokenize("()").expect("Unexpected tokenize error");
    let ast = parse(&tokens).expect("Unexpected parse error");
    let expected_ast = {
        let mut expected_ast = Ast::new();
        let root_handle = expected_ast.add_root(Rule::Expression, 0, 2);
        let paren_expression =
            expected_ast.add_child(root_handle, Rule::ParenExpression, 0, 2);
        let expression_handle =
            expected_ast.add_child(paren_expression, Rule::Expression, 1, 0);
        expected_ast.add_terminal_child(expression_handle, None, 1, 0);
        expected_ast
    };

    check_ast_equal(&ast, &expected_ast);
}

#[test]
fn empty_statement() {
    let tokens = tokenize(";").expect("Unexpected tokenize error");
    match parse(&tokens) {
        Ok(_) => assert!(false),
        Err(errors) => {
            assert_eq!(errors.len(), 1);

            let error = errors.get(0).expect("Empty statement error");
            assert_eq!(error.start_line, 1);
            assert_eq!(error.end_line, 1);
        }
    }
}

#[test]
fn empty_statement_in_braces() {
    // TODO: maybe this should raise a warning?
    let tokens = tokenize("{;}").expect("Unexpected tokenize error");
    let ast = parse(&tokens).expect("Unexpected parse error");
    let expected_ast = {
        let mut expected_ast = Ast::new();
        let root_handle = expected_ast.add_root(Rule::Expression, 0, 3);
        let brace_expression_handle =
            expected_ast.add_child(root_handle, Rule::BraceExpression, 0, 3);

        // statements
        {
            let statements_handle = expected_ast.add_child(
                brace_expression_handle,
                Rule::BraceStatements,
                1,
                1,
            );
            let statement_handle = expected_ast.add_child(
                statements_handle,
                Rule::Statement,
                1,
                1,
            );
            // statement expression
            let expression_handle = expected_ast.add_child(
                statement_handle,
                Rule::Expression,
                1,
                0,
            );
            expected_ast.add_terminal_child(expression_handle, None, 1, 0);
        }

        // end expression
        let expression_handle = expected_ast.add_child(
            brace_expression_handle,
            Rule::Expression,
            2,
            0,
        );
        expected_ast.add_terminal_child(expression_handle, None, 2, 0);

        expected_ast
    };

    check_ast_equal(&ast, &expected_ast);
}

/// test for mismatched parens
#[test]
fn paren_mismatch() {
    let tokens = tokenize("(open").expect("Unexpected tokenize error");
    match parse(&tokens) {
        Ok(_) => assert!(false),
        Err(errors) => {
            assert_eq!(errors.len(), 1);
            let error = errors.get(0).expect("Unexpected missing error");
            assert_eq!(error.start_line, 1);
            assert_eq!(error.end_line, 1);
        }
    };
}

#[test]
fn brace_mismatch() {
    let tokens = tokenize("{open").expect("Unexpected tokenize error");
    match parse(&tokens) {
        Ok(_) => assert!(false),
        Err(errors) => {
            assert_eq!(errors.len(), 1);
            let error = errors.get(0).expect("Unexpected missing error");
            assert_eq!(error.start_line, 1);
            assert_eq!(error.end_line, 1);
        }
    };
}

/// test the parse of a single basic token
#[test]
fn single_token() {
    let tokens = tokenize("0").expect("Unexpected tokenize error");
    let ast = parse(&tokens).expect("Unexpected parse error");
    let expected_ast = {
        let mut expected_ast = Ast::new();
        let root_handle = expected_ast.add_root(Rule::Expression, 0, 1);
        expected_ast.add_terminal_child(
            root_handle,
            Some(Token::IntLiteral(0)),
            0,
            1,
        );
        expected_ast
    };

    check_ast_equal(&ast, &expected_ast);
}

#[test]
fn single_token_in_braces() {
    let tokens = tokenize("{0}").expect("Unexpected tokenize error");
    let ast = parse(&tokens).expect("Unexpected parse error");
    let expected_ast = {
        let mut expected_ast = Ast::new();
        let root_handle = expected_ast.add_root(Rule::Expression, 0, 3);
        let brace_expression_handle =
            expected_ast.add_child(root_handle, Rule::BraceExpression, 0, 3);
        let expression_handle = expected_ast.add_child(
            brace_expression_handle,
            Rule::Expression,
            1,
            1,
        );
        expected_ast.add_terminal_child(
            expression_handle,
            Some(Token::IntLiteral(0)),
            1,
            1,
        );
        expected_ast
    };

    check_ast_equal(&ast, &expected_ast);
}

#[test]
fn single_token_nested_braces() {
    let tokens = tokenize("{{0}}").expect("Unexpected tokenize error");
    let ast = parse(&tokens).expect("Unexpected parse error");

    let expected_ast = {
        let mut expected_ast = Ast::new();
        let root_handle = expected_ast.add_root(Rule::Expression, 0, 5);

        let outer_brace_expression_handle =
            expected_ast.add_child(root_handle, Rule::BraceExpression, 0, 5);
        let expression_handle = expected_ast.add_child(
            outer_brace_expression_handle,
            Rule::Expression,
            1,
            3,
        );

        let brace_expression_handle = expected_ast.add_child(
            expression_handle,
            Rule::BraceExpression,
            1,
            3,
        );
        let expression_handle = expected_ast.add_child(
            brace_expression_handle,
            Rule::Expression,
            2,
            1,
        );
        expected_ast.add_terminal_child(
            expression_handle,
            Some(Token::IntLiteral(0)),
            2,
            1,
        );
        expected_ast
    };

    check_ast_equal(&ast, &expected_ast);
}

/// test a simple arithmetic expression
#[test]
fn arithmetic_expression() {
    let tokens = tokenize("1 + 2").expect("Unexpected tokenize error");
    let ast = parse(&tokens).expect("Unexpected parse error");

    let expected_ast = {
        let mut expected_ast = Ast::new();
        let root_handle = expected_ast.add_root(Rule::Expression, 0, 3);
        add_expected_add_child(
            &mut expected_ast,
            root_handle,
            Token::IntLiteral(1),
            0,
            Token::IntLiteral(2),
        );
        expected_ast
    };

    check_ast_equal(&ast, &expected_ast);
}

/// test a simple multiplication expression
#[test]
fn mult_expression() {
    let tokens = tokenize("1 * 2").expect("Unexpected tokenize error");
    let ast = parse(&tokens).expect("Unexpected parse error");

    let expected_ast = {
        let mut expected_ast = Ast::new();
        let root_handle = expected_ast.add_root(Rule::Expression, 0, 3);
        let mult_div_handle = expected_ast.add_child_with_data(
            root_handle,
            Rule::MultDiv,
            Some(Token::Times),
            0,
            3,
        );
        // 1 (recursive)
        expected_ast.add_terminal_child(
            mult_div_handle,
            Some(Token::IntLiteral(1)),
            0,
            1,
        );
        // 2
        expected_ast.add_terminal_child(
            mult_div_handle,
            Some(Token::IntLiteral(2)),
            2,
            1,
        );
        expected_ast
    };

    check_ast_equal(&ast, &expected_ast);
}

/// test for group on right
#[test]
fn expression_with_grouping_right() {
    let tokens = tokenize("1 + (2 + 3)").expect("Unexpected tokenize error");
    let ast = parse(&tokens).expect("Unexpected parse error");

    let expected_ast = {
        let mut expected_ast = Ast::new();
        let root_handle = expected_ast.add_root(Rule::Expression, 0, 7);
        let plus_minus_handle = expected_ast.add_child_with_data(
            root_handle,
            Rule::PlusMinus,
            Some(Token::Plus),
            0,
            7,
        );

        // LHS: 1
        expected_ast.add_terminal_child(
            plus_minus_handle,
            Some(Token::IntLiteral(1)),
            0,
            1,
        );

        // RHS: (2 + 3)
        {
            let paren_expression = expected_ast.add_child(
                plus_minus_handle,
                Rule::ParenExpression,
                2,
                5,
            );
            // 2 + 3
            let expression_handle = expected_ast.add_child(
                paren_expression,
                Rule::Expression,
                3,
                3,
            );
            add_expected_add_child(
                &mut expected_ast,
                expression_handle,
                Token::IntLiteral(2),
                3,
                Token::IntLiteral(3),
            );
        }

        expected_ast
    };

    check_ast_equal(&ast, &expected_ast);
}

/// test for group on left
#[test]
fn expression_with_grouping_left() {
    let tokens = tokenize("(1 + 2) * 3").expect("Unexpected tokenize error");
    let ast = parse(&tokens).expect("Unexpected parse error");

    let expected_ast = {
        let mut expected_ast = Ast::new();
        let root_handle = expected_ast.add_root(Rule::Expression, 0, 7);
        let mult_div_handle = expected_ast.add_child_with_data(
            root_handle,
            Rule::MultDiv,
            Some(Token::Times),
            0,
            7,
        );

        // LHS: (1 + 2)
        {
            let paren_expression = expected_ast.add_child(
                mult_div_handle,
                Rule::ParenExpression,
                0,
                5,
            );
            let expression_handle = expected_ast.add_child(
                paren_expression,
                Rule::Expression,
                1,
                3,
            );
            add_expected_add_child(
                &mut expected_ast,
                expression_handle,
                Token::IntLiteral(1),
                1,
                Token::IntLiteral(2),
            );
        }

        // RHS: 3
        expected_ast.add_terminal_child(
            mult_div_handle,
            Some(Token::IntLiteral(3)),
            6,
            1,
        );

        expected_ast
    };

    check_ast_equal(&ast, &expected_ast);
}

#[test]
fn expression_with_brace_grouping() {
    let tokens = tokenize("1 + {2 + 3}").expect("Unexpected tokenize error");
    let ast = parse(&tokens).expect("Unexpected parse error");

    let expected_ast = {
        let mut expected_ast = Ast::new();
        let root_handle = expected_ast.add_root(Rule::Expression, 0, 7);
        let plus_minus_handle = expected_ast.add_child_with_data(
            root_handle,
            Rule::PlusMinus,
            Some(Token::Plus),
            0,
            7,
        );

        // LHS: 1
        expected_ast.add_terminal_child(
            plus_minus_handle,
            Some(Token::IntLiteral(1)),
            0,
            1,
        );

        // RHS: {2 + 3}
        {
            // 2 + 3
            let brace_expression_handle = expected_ast.add_child(
                plus_minus_handle,
                Rule::BraceExpression,
                2,
                5,
            );
            let expression_handle = expected_ast.add_child(
                brace_expression_handle,
                Rule::Expression,
                3,
                3,
            );
            add_expected_add_child(
                &mut expected_ast,
                expression_handle,
                Token::IntLiteral(2),
                3,
                Token::IntLiteral(3),
            );
        }

        expected_ast
    };

    check_ast_equal(&ast, &expected_ast);
}

/// test an expression including symbols as opposed to literals
#[test]
fn expression_with_symbols() {
    let tokens = tokenize("a + b").expect("Unexpected tokenize error");
    let ast = parse(&tokens).expect("Unexpected parse error");

    let expected_ast = {
        let mut expected_ast = Ast::new();
        let root_handle = expected_ast.add_root(Rule::Expression, 0, 3);
        add_expected_add_child(
            &mut expected_ast,
            root_handle,
            Token::Symbol("a".to_owned()),
            0,
            Token::Symbol("b".to_owned()),
        );
        expected_ast
    };

    check_ast_equal(&ast, &expected_ast);
}

/// a basic test for assignment
#[test]
fn assignment_symbol() {
    let tokens = tokenize("{ a = b; }").expect("Unexpected tokenize error");
    let ast = parse(&tokens).expect("Unexpected parse error");

    let expected_ast = {
        let mut expected_ast = Ast::new();
        let root_handle = expected_ast.add_root(Rule::Expression, 0, 6);
        let brace_expression_handle =
            expected_ast.add_child(root_handle, Rule::BraceExpression, 0, 6);

        // statements
        {
            let statements_handle = expected_ast.add_child(
                brace_expression_handle,
                Rule::BraceStatements,
                1,
                4,
            );
            add_assignment_statement(
                &mut expected_ast,
                statements_handle,
                Token::Symbol("a".to_owned()),
                1,
                Token::Symbol("b".to_owned()),
            );
        }

        // no expression at end of brace expression
        add_terminal_expression(
            &mut expected_ast,
            brace_expression_handle,
            None,
            5,
            0,
        );

        expected_ast
    };

    check_ast_equal(&ast, &expected_ast);
}

#[test]
fn brace_expression_with_variable_only() {
    let tokens = tokenize("{ a }").expect("Unexpected tokenize error");
    let ast = parse(&tokens).expect("Unexpected parse error");

    let expected_ast = {
        let mut expected_ast = Ast::new();
        let root_handle = expected_ast.add_root(Rule::Expression, 0, 3);
        let brace_expression_handle =
            expected_ast.add_child(root_handle, Rule::BraceExpression, 0, 3);

        // no expression at end of brace expression
        add_terminal_expression(
            &mut expected_ast,
            brace_expression_handle,
            Some(Token::Symbol("a".to_owned())),
            1,
            1,
        );

        expected_ast
    };

    check_ast_equal(&ast, &expected_ast);
}

#[test]
fn brace_expression_with_expression_only() {
    let tokens = tokenize("{ a + b }").expect("Unexpected tokenize error");
    let ast = parse(&tokens).expect("Unexpected parse error");

    let expected_ast = {
        let mut expected_ast = Ast::new();
        let root_handle = expected_ast.add_root(Rule::Expression, 0, 5);
        let brace_expression_handle =
            expected_ast.add_child(root_handle, Rule::BraceExpression, 0, 5);

        let expression_handle = expected_ast.add_child(
            brace_expression_handle,
            Rule::Expression,
            1,
            3,
        );

        add_expected_add_child(
            &mut expected_ast,
            expression_handle,
            Token::Symbol("a".to_owned()),
            1,
            Token::Symbol("b".to_owned()),
        );

        expected_ast
    };

    check_ast_equal(&ast, &expected_ast);
}

#[test]
fn brace_expression_with_variable_statement_only() {
    let tokens = tokenize("{ a; }").expect("Unexpected tokenize error");
    let ast = parse(&tokens).expect("Unexpected parse error");

    let expected_ast = {
        let mut expected_ast = Ast::new();
        let root_handle = expected_ast.add_root(Rule::Expression, 0, 4);
        let brace_expression_handle =
            expected_ast.add_child(root_handle, Rule::BraceExpression, 0, 4);

        // statements
        {
            let statements_handle = expected_ast.add_child(
                brace_expression_handle,
                Rule::BraceStatements,
                1,
                2,
            );
            let statement_handle = expected_ast.add_child(
                statements_handle,
                Rule::Statement,
                1,
                2,
            );

            let expression_handle = expected_ast.add_child(
                statement_handle,
                Rule::Expression,
                1,
                1,
            );
            expected_ast.add_terminal_child(
                expression_handle,
                Some(Token::Symbol("a".to_owned())),
                1,
                1,
            );
        }

        // no expression at end of brace expression
        add_terminal_expression(
            &mut expected_ast,
            brace_expression_handle,
            None,
            3,
            0,
        );

        expected_ast
    };

    check_ast_equal(&ast, &expected_ast);
}

#[test]
fn brace_expression_statement_only() {
    let tokens = tokenize("{ a + b; }").expect("Unexpected tokenize error");
    let ast = parse(&tokens).expect("Unexpected parse error");

    let expected_ast = {
        let mut expected_ast = Ast::new();
        let root_handle = expected_ast.add_root(Rule::Expression, 0, 6);
        let brace_expression_handle =
            expected_ast.add_child(root_handle, Rule::BraceExpression, 0, 6);

        // statements
        {
            let statements_handle = expected_ast.add_child(
                brace_expression_handle,
                Rule::BraceStatements,
                1,
                4,
            );
            let statement_handle = expected_ast.add_child(
                statements_handle,
                Rule::Statement,
                1,
                4,
            );

            // a + b
            let expression_handle = expected_ast.add_child(
                statement_handle,
                Rule::Expression,
                1,
                3,
            );
            add_expected_add_child(
                &mut expected_ast,
                expression_handle,
                Token::Symbol("a".to_owned()),
                1,
                Token::Symbol("b".to_owned()),
            );
        }

        // no expression at end of braces
        add_terminal_expression(
            &mut expected_ast,
            brace_expression_handle,
            None,
            5,
            0,
        );

        expected_ast
    };

    check_ast_equal(&ast, &expected_ast);
}

#[test]
fn assign_expression() {
    let tokens = tokenize("{ a = b + c; }").expect("Unexpected tokenize error");
    let ast = parse(&tokens).expect("Unexpected parse error");

    let expected_ast = {
        let mut expected_ast = Ast::new();
        let root_handle = expected_ast.add_root(Rule::Expression, 0, 8);
        let brace_expression_handle =
            expected_ast.add_child(root_handle, Rule::BraceExpression, 0, 8);

        // statements
        {
            let statements_handle = expected_ast.add_child(
                brace_expression_handle,
                Rule::BraceStatements,
                1,
                6,
            );
            let statement_handle = expected_ast.add_child(
                statements_handle,
                Rule::Statement,
                1,
                6,
            );

            // a = b + c
            {
                // lhs: a
                add_terminal_expression(
                    &mut expected_ast,
                    statement_handle,
                    Some(Token::Symbol("a".to_owned())),
                    1,
                    1,
                );

                // rhs: b + c
                {
                    let expression_handle = expected_ast.add_child(
                        statement_handle,
                        Rule::Expression,
                        3,
                        3,
                    );
                    add_expected_add_child(
                        &mut expected_ast,
                        expression_handle,
                        Token::Symbol("b".to_owned()),
                        3,
                        Token::Symbol("c".to_owned()),
                    );
                }
            }
        }

        // no expression at end of braces
        add_terminal_expression(
            &mut expected_ast,
            brace_expression_handle,
            None,
            7,
            0,
        );

        expected_ast
    };

    check_ast_equal(&ast, &expected_ast);
}

#[test]
fn assign_brace_expression() {
    let tokens =
        tokenize("{ a = {b + c}; }").expect("Unexpected tokenize error");
    let ast = parse(&tokens).expect("Unexpected parse error");

    let expected_ast = {
        let mut expected_ast = Ast::new();

        let root_handle = expected_ast.add_root(Rule::Expression, 0, 10);
        let brace_expression_handle =
            expected_ast.add_child(root_handle, Rule::BraceExpression, 0, 10);

        // statements
        {
            let brace_statements_handle = expected_ast.add_child(
                brace_expression_handle,
                Rule::BraceStatements,
                1,
                8,
            );
            let statement_handle = expected_ast.add_child(
                brace_statements_handle,
                Rule::Statement,
                1,
                8,
            );

            // lhs: a
            add_terminal_expression(
                &mut expected_ast,
                statement_handle,
                Some(Token::Symbol("a".to_owned())),
                1,
                1,
            );

            // rhs: {b + c}
            {
                let expression_handle = expected_ast.add_child(
                    statement_handle,
                    Rule::Expression,
                    3,
                    5,
                );
                let brace_expression_handle = expected_ast.add_child(
                    expression_handle,
                    Rule::BraceExpression,
                    3,
                    5,
                );

                // expression
                {
                    let rhs_expression_handle = expected_ast.add_child(
                        brace_expression_handle,
                        Rule::Expression,
                        4,
                        3,
                    );
                    add_expected_add_child(
                        &mut expected_ast,
                        rhs_expression_handle,
                        Token::Symbol("b".to_owned()),
                        4,
                        Token::Symbol("c".to_owned()),
                    );
                }
            }
        }

        // expression
        add_terminal_expression(
            &mut expected_ast,
            brace_expression_handle,
            None,
            9,
            0,
        );

        expected_ast
    };

    check_ast_equal(&ast, &expected_ast);
}

#[test]
fn assign_brace_expression_with_statements() {
    let tokens = tokenize("{ a = {b = c + d; a + b}; }")
        .expect("Unexpected tokenize error");
    let ast = parse(&tokens).expect("Unexpected parse error");

    let expected_ast = {
        let mut expected_ast = Ast::new();

        let root_handle = expected_ast.add_root(Rule::Expression, 0, 16);
        let brace_expression_handle =
            expected_ast.add_child(root_handle, Rule::BraceExpression, 0, 16);

        // statements
        {
            let brace_statements_handle = expected_ast.add_child(
                brace_expression_handle,
                Rule::BraceStatements,
                1,
                14,
            );
            let statement_handle = expected_ast.add_child(
                brace_statements_handle,
                Rule::Statement,
                1,
                14,
            );

            // lhs: a
            add_terminal_expression(
                &mut expected_ast,
                statement_handle,
                Some(Token::Symbol("a".to_owned())),
                1,
                1,
            );

            // rhs: {b = c + d; a + b}
            {
                let expression_handle = expected_ast.add_child(
                    statement_handle,
                    Rule::Expression,
                    3,
                    11,
                );
                let brace_expression_handle = expected_ast.add_child(
                    expression_handle,
                    Rule::BraceExpression,
                    3,
                    11,
                );

                // statements
                {
                    let brace_statements_handle = expected_ast.add_child(
                        brace_expression_handle,
                        Rule::BraceStatements,
                        4,
                        6,
                    );
                    let statement_handle = expected_ast.add_child(
                        brace_statements_handle,
                        Rule::Statement,
                        4,
                        6,
                    );

                    // b = c + d;
                    {
                        // lhs
                        add_terminal_expression(
                            &mut expected_ast,
                            statement_handle,
                            Some(Token::Symbol("b".to_owned())),
                            4,
                            1,
                        );

                        // rhs
                        {
                            let expression_handle = expected_ast.add_child(
                                statement_handle,
                                Rule::Expression,
                                6,
                                3,
                            );
                            add_expected_add_child(
                                &mut expected_ast,
                                expression_handle,
                                Token::Symbol("c".to_owned()),
                                6,
                                Token::Symbol("d".to_owned()),
                            );
                        }
                    }
                }

                // expression: a + b
                {
                    let expression_handle = expected_ast.add_child(
                        brace_expression_handle,
                        Rule::Expression,
                        10,
                        3,
                    );
                    add_expected_add_child(
                        &mut expected_ast,
                        expression_handle,
                        Token::Symbol("a".to_owned()),
                        10,
                        Token::Symbol("b".to_owned()),
                    );
                }
            }
        }

        // expression
        add_terminal_expression(
            &mut expected_ast,
            brace_expression_handle,
            None,
            15,
            0,
        );

        expected_ast
    };

    check_ast_equal(&ast, &expected_ast);
}

#[test]
fn left_right_precedence() {
    let tokens = tokenize("a + b - c").expect("Unexpected tokenize error");
    let ast = parse(&tokens).expect("Unexpected parse error");

    let expected_ast = {
        let mut expected_ast = Ast::new();
        let root_handle = expected_ast.add_root(Rule::Expression, 0, 5);
        let plus_minus_handle = expected_ast.add_child_with_data(
            root_handle,
            Rule::PlusMinus,
            Some(Token::Minus),
            0,
            5,
        );

        // a + b
        {
            let a_plus_b_handle = expected_ast.add_child_with_data(
                plus_minus_handle,
                Rule::PlusMinus,
                Some(Token::Plus),
                0,
                3,
            );

            // a
            expected_ast.add_terminal_child(
                a_plus_b_handle,
                Some(Token::Symbol("a".to_owned())),
                0,
                1,
            );

            // b
            expected_ast.add_terminal_child(
                a_plus_b_handle,
                Some(Token::Symbol("b".to_owned())),
                2,
                1,
            );
        }

        // - c
        expected_ast.add_terminal_child(
            plus_minus_handle,
            Some(Token::Symbol("c".to_owned())),
            4,
            1,
        );

        expected_ast
    };

    check_ast_equal(&ast, &expected_ast);
}

#[test]
fn add_mult_precedence() {
    let tokens = tokenize("a + b * c").expect("Unexpected tokenize error");
    let ast = parse(&tokens).expect("Unexpected parse error");

    let expected_ast = {
        let mut expected_ast = Ast::new();
        let root_handle = expected_ast.add_root(Rule::Expression, 0, 5);
        let plus_minus_handle = expected_ast.add_child_with_data(
            root_handle,
            Rule::PlusMinus,
            Some(Token::Plus),
            0,
            5,
        );

        // a
        expected_ast.add_terminal_child(
            plus_minus_handle,
            Some(Token::Symbol("a".to_owned())),
            0,
            1,
        );

        // b * c
        {
            let mult_div_handle = expected_ast.add_child_with_data(
                plus_minus_handle,
                Rule::MultDiv,
                Some(Token::Times),
                2,
                3,
            );

            // b
            expected_ast.add_terminal_child(
                mult_div_handle,
                Some(Token::Symbol("b".to_owned())),
                2,
                1,
            );

            // c
            expected_ast.add_terminal_child(
                mult_div_handle,
                Some(Token::Symbol("c".to_owned())),
                4,
                1,
            );
        }
        expected_ast
    };

    check_ast_equal(&ast, &expected_ast);
}

#[test]
fn nested_groups() {
    let tokens =
        tokenize("a * (b - (c + d))").expect("Unexpected tokenize error");
    let ast = parse(&tokens).expect("Unexpected parse error");
    let expected_ast = {
        let mut expected_ast = Ast::new();

        let root_handle = expected_ast.add_root(Rule::Expression, 0, 11);

        let mult_div_handle = expected_ast.add_child_with_data(
            root_handle,
            Rule::MultDiv,
            Some(Token::Times),
            0,
            11,
        );

        // LHS: a
        expected_ast.add_terminal_child(
            mult_div_handle,
            Some(Token::Symbol("a".to_owned())),
            0,
            1,
        );

        // RHS: (b - (c + d))
        {
            let paren_expression_handle = expected_ast.add_child(
                mult_div_handle,
                Rule::ParenExpression,
                2,
                9,
            );
            let expression_handle = expected_ast.add_child(
                paren_expression_handle,
                Rule::Expression,
                3,
                7,
            );
            let plus_minus_handle = expected_ast.add_child_with_data(
                expression_handle,
                Rule::PlusMinus,
                Some(Token::Minus),
                3,
                7,
            );

            // LHS: b
            expected_ast.add_terminal_child(
                plus_minus_handle,
                Some(Token::Symbol("b".to_owned())),
                3,
                1,
            );

            // RHS: (c + d)
            {
                let paren_expression_handle = expected_ast.add_child(
                    plus_minus_handle,
                    Rule::ParenExpression,
                    5,
                    5,
                );
                let expression_handle = expected_ast.add_child(
                    paren_expression_handle,
                    Rule::Expression,
                    6,
                    3,
                );
                let plus_minus_handle = expected_ast.add_child_with_data(
                    expression_handle,
                    Rule::PlusMinus,
                    Some(Token::Plus),
                    6,
                    3,
                );

                // LHS: c
                expected_ast.add_terminal_child(
                    plus_minus_handle,
                    Some(Token::Symbol("c".to_owned())),
                    6,
                    1,
                );

                // RHS: d
                expected_ast.add_terminal_child(
                    plus_minus_handle,
                    Some(Token::Symbol("d".to_owned())),
                    8,
                    1,
                );
            }
        }

        expected_ast
    };
    check_ast_equal(&ast, &expected_ast);
}

#[test]
fn nested_brace_expressions() {
    let tokens = tokenize(
        "
        {
            a = b;
            c = {
                d = 2 * a;
                d
            };
            a + c
        }
    ",
    )
    .expect("Unexpected tokenize error");
    let ast = parse(&tokens).expect("Unexpected parse error");

    let expected_ast = {
        let mut expected_ast = Ast::new();
        let root_handle = expected_ast.add_root(Rule::Expression, 0, 21);
        let brace_expression_handle =
            expected_ast.add_child(root_handle, Rule::BraceExpression, 0, 21);

        // statements
        {
            let statements_handle = expected_ast.add_child(
                brace_expression_handle,
                Rule::BraceStatements,
                1,
                16,
            );

            // a = b;
            {
                let statements_handle = expected_ast.add_child(
                    statements_handle,
                    Rule::BraceStatements,
                    1,
                    4,
                );

                // a = b;
                add_assignment_statement(
                    &mut expected_ast,
                    statements_handle,
                    Token::Symbol("a".to_owned()),
                    1,
                    Token::Symbol("b".to_owned()),
                );
            }

            /*
            c = {
                d = 2 * a;
                d
            };
            */
            {
                let statement_handle = expected_ast.add_child(
                    statements_handle,
                    Rule::Statement,
                    5,
                    12,
                );

                // LHS: c
                add_terminal_expression(
                    &mut expected_ast,
                    statement_handle,
                    Some(Token::Symbol("c".to_owned())),
                    5,
                    1,
                );

                // RHS
                /*
                {
                    d = 2 * a;
                    d
                };
                */
                {
                    let expression_handle = expected_ast.add_child(
                        statement_handle,
                        Rule::Expression,
                        7,
                        9,
                    );
                    let brace_expression_handle = expected_ast.add_child(
                        expression_handle,
                        Rule::BraceExpression,
                        7,
                        9,
                    );

                    {
                        let statements_handle = expected_ast.add_child(
                            brace_expression_handle,
                            Rule::BraceStatements,
                            8,
                            6,
                        );

                        // d = 2 * a;
                        {
                            let statement_handle = expected_ast.add_child(
                                statements_handle,
                                Rule::Statement,
                                8,
                                6,
                            );

                            // LHS: d
                            add_terminal_expression(
                                &mut expected_ast,
                                statement_handle,
                                Some(Token::Symbol("d".to_owned())),
                                8,
                                1,
                            );

                            // RHS: 2 * a
                            let expression_handle = expected_ast.add_child(
                                statement_handle,
                                Rule::Expression,
                                10,
                                3,
                            );
                            add_expected_mult_child(
                                &mut expected_ast,
                                expression_handle,
                                Token::IntLiteral(2),
                                10,
                                Token::Symbol("a".to_owned()),
                            );
                        }
                    }

                    // expression at end
                    add_terminal_expression(
                        &mut expected_ast,
                        brace_expression_handle,
                        Some(Token::Symbol("d".to_owned())),
                        14,
                        1,
                    );
                }
            }
        }

        // a + c
        {
            let end_expression_handle = expected_ast.add_child(
                brace_expression_handle,
                Rule::Expression,
                17,
                3,
            );
            add_expected_add_child(
                &mut expected_ast,
                end_expression_handle,
                Token::Symbol("a".to_owned()),
                17,
                Token::Symbol("c".to_owned()),
            );
        }

        expected_ast
    };

    check_ast_equal(&ast, &expected_ast);
}

#[test]
fn nested_brace_expressions_brace_first() {
    let tokens = tokenize(
        "
        {
            c = {
                d = 2 * a;
                d
            };
            a = b;
            a + c
        }
    ",
    )
    .expect("Unexpected tokenize error");
    let ast = parse(&tokens).expect("Unexpected parse error");
    let expected_ast = {
        let mut expected_ast = Ast::new();
        let root_handle = expected_ast.add_root(Rule::Expression, 0, 21);
        let brace_expression_handle =
            expected_ast.add_child(root_handle, Rule::BraceExpression, 0, 21);

        // statements
        {
            let statements_handle = expected_ast.add_child(
                brace_expression_handle,
                Rule::BraceStatements,
                1,
                16,
            );

            /*
            c = {
                d = 2 * a;
                d
            };
            */
            {
                let statements_handle = expected_ast.add_child(
                    statements_handle,
                    Rule::BraceStatements,
                    1,
                    12,
                );

                let statement_handle = expected_ast.add_child(
                    statements_handle,
                    Rule::Statement,
                    1,
                    12,
                );

                // LHS: c
                add_terminal_expression(
                    &mut expected_ast,
                    statement_handle,
                    Some(Token::Symbol("c".to_owned())),
                    1,
                    1,
                );

                // RHS
                /*
                {
                    d = 2 * a;
                    d
                }
                */
                {
                    let expression_handle = expected_ast.add_child(
                        statement_handle,
                        Rule::Expression,
                        3,
                        9,
                    );
                    let brace_expression_handle = expected_ast.add_child(
                        expression_handle,
                        Rule::BraceExpression,
                        3,
                        9,
                    );

                    {
                        let statements_handle = expected_ast.add_child(
                            brace_expression_handle,
                            Rule::BraceStatements,
                            4,
                            6,
                        );

                        // d = 2 * a;
                        {
                            let statement_handle = expected_ast.add_child(
                                statements_handle,
                                Rule::Statement,
                                4,
                                6,
                            );

                            // LHS: d
                            add_terminal_expression(
                                &mut expected_ast,
                                statement_handle,
                                Some(Token::Symbol("d".to_owned())),
                                4,
                                1,
                            );

                            // RHS: 2 * a
                            let expression_handle = expected_ast.add_child(
                                statement_handle,
                                Rule::Expression,
                                6,
                                3,
                            );
                            add_expected_mult_child(
                                &mut expected_ast,
                                expression_handle,
                                Token::IntLiteral(2),
                                6,
                                Token::Symbol("a".to_owned()),
                            );
                        }
                    }

                    // expression at end
                    add_terminal_expression(
                        &mut expected_ast,
                        brace_expression_handle,
                        Some(Token::Symbol("d".to_owned())),
                        10,
                        1,
                    );
                }
            }

            // a = b;
            {
                // a = b;
                add_assignment_statement(
                    &mut expected_ast,
                    statements_handle,
                    Token::Symbol("a".to_owned()),
                    13,
                    Token::Symbol("b".to_owned()),
                );
            }
        }

        // a + c
        {
            let end_expression_handle = expected_ast.add_child(
                brace_expression_handle,
                Rule::Expression,
                17,
                3,
            );
            add_expected_add_child(
                &mut expected_ast,
                end_expression_handle,
                Token::Symbol("a".to_owned()),
                17,
                Token::Symbol("c".to_owned()),
            );
        }

        expected_ast
    };

    check_ast_equal(&ast, &expected_ast);
}

#[test]
fn if_only() {
    let tokens = tokenize("if (a + b) == c {d = c;}")
        .expect("Unexpected tokenize error");
    let ast = parse(&tokens).expect("Unexpected parse error");
    let expected_ast = {
        let mut expected_ast = Ast::new();
        let root_handle = expected_ast.add_root(Rule::Expression, 0, 14);
        let if_else_handle =
            expected_ast.add_child(root_handle, Rule::IfElse, 0, 14);
        // condition expression
        {
            let condition_expression_handle =
                expected_ast.add_child(if_else_handle, Rule::Expression, 1, 7);
            let equality_handle = expected_ast.add_child_with_data(
                condition_expression_handle,
                Rule::Equality,
                Some(Token::BoolEquals),
                1,
                7,
            );
            // (a + b)
            {
                let paren_expression = expected_ast.add_child(
                    equality_handle,
                    Rule::ParenExpression,
                    1,
                    5,
                );
                let expression_handle = expected_ast.add_child(
                    paren_expression,
                    Rule::Expression,
                    2,
                    3,
                );
                add_expected_add_child(
                    &mut expected_ast,
                    expression_handle,
                    Token::Symbol("a".to_owned()),
                    2,
                    Token::Symbol("b".to_owned()),
                );
            }
            // c
            expected_ast.add_terminal_child(
                equality_handle,
                Some(Token::Symbol("c".to_owned())),
                7,
                1,
            );
        }
        // executed brace_expression
        {
            let brace_expression = expected_ast.add_child(
                if_else_handle,
                Rule::BraceExpression,
                8,
                6,
            );
            // brace statements
            {
                let brace_statements_handle = expected_ast.add_child(
                    brace_expression,
                    Rule::BraceStatements,
                    9,
                    4,
                );
                add_assignment_statement(
                    &mut expected_ast,
                    brace_statements_handle,
                    Token::Symbol("d".to_owned()),
                    9,
                    Token::Symbol("c".to_owned()),
                );
            }

            // no ending expression
            add_terminal_expression(
                &mut expected_ast,
                brace_expression,
                None,
                13,
                0,
            );
        }
        expected_ast
    };

    check_ast_equal(&ast, &expected_ast);
}

#[test]
fn if_else() {
    let tokens = tokenize(
        "
        if (a + b) == c {
            d = c;
        } else {
            d = e;
        }
    ",
    )
    .expect("Unexpected tokenize error");
    let ast = parse(&tokens).expect("Unexpected parse error");
    let expected_ast = {
        let mut expected_ast = Ast::new();
        let root_handle = expected_ast.add_root(Rule::Expression, 0, 21);
        let if_else_handle =
            expected_ast.add_child(root_handle, Rule::IfElse, 0, 21);

        // condition expression
        {
            let condition_expression_handle =
                expected_ast.add_child(if_else_handle, Rule::Expression, 1, 7);
            let equality_handle = expected_ast.add_child_with_data(
                condition_expression_handle,
                Rule::Equality,
                Some(Token::BoolEquals),
                1,
                7,
            );
            // (a + b)
            {
                let paren_expression = expected_ast.add_child(
                    equality_handle,
                    Rule::ParenExpression,
                    1,
                    5,
                );
                let expression_handle = expected_ast.add_child(
                    paren_expression,
                    Rule::Expression,
                    2,
                    3,
                );
                add_expected_add_child(
                    &mut expected_ast,
                    expression_handle,
                    Token::Symbol("a".to_owned()),
                    2,
                    Token::Symbol("b".to_owned()),
                );
            }
            // c
            expected_ast.add_terminal_child(
                equality_handle,
                Some(Token::Symbol("c".to_owned())),
                7,
                1,
            );
        }
        // executed brace_expression
        {
            let brace_expression = expected_ast.add_child(
                if_else_handle,
                Rule::BraceExpression,
                8,
                6,
            );
            // brace statements
            {
                let brace_statements_handle = expected_ast.add_child(
                    brace_expression,
                    Rule::BraceStatements,
                    9,
                    4,
                );
                add_assignment_statement(
                    &mut expected_ast,
                    brace_statements_handle,
                    Token::Symbol("d".to_owned()),
                    9,
                    Token::Symbol("c".to_owned()),
                );
            }

            // no ending expression
            add_terminal_expression(
                &mut expected_ast,
                brace_expression,
                None,
                13,
                0,
            );
        }

        // else
        {
            let expression_handle =
                expected_ast.add_child(if_else_handle, Rule::Expression, 15, 6);
            let brace_expression_handle = expected_ast.add_child(
                expression_handle,
                Rule::BraceExpression,
                15,
                6,
            );

            // statements
            let brace_statements_handle = expected_ast.add_child(
                brace_expression_handle,
                Rule::BraceStatements,
                16,
                4,
            );
            add_assignment_statement(
                &mut expected_ast,
                brace_statements_handle,
                Token::Symbol("d".to_owned()),
                16,
                Token::Symbol("e".to_owned()),
            );
            // expression
            add_terminal_expression(
                &mut expected_ast,
                brace_expression_handle,
                None,
                20,
                0,
            );
        }
        expected_ast
    };

    check_ast_equal(&ast, &expected_ast);
}

#[test]
fn if_else_if() {
    let tokens = tokenize(
        "
        if a == b {
            d = b;
        } else if a == c {
            d = c;
        } else {
            d = e;
        }
    ",
    )
    .expect("Unexpected tokenize error");
    let ast = parse(&tokens).expect("Unexpected parse error");

    let expected_ast = {
        let mut expected_ast = Ast::new();
        let root_handle = expected_ast.add_root(Rule::Expression, 0, 28);
        let if_else_handle =
            expected_ast.add_child(root_handle, Rule::IfElse, 0, 28);

        // a == b
        {
            let condition_expression_handle =
                expected_ast.add_child(if_else_handle, Rule::Expression, 1, 3);

            add_comparison_tree(
                &mut expected_ast,
                condition_expression_handle,
                Token::Symbol("a".to_owned()),
                1,
                Token::Symbol("b".to_owned()),
            );
        }
        // { d = b; }
        {
            let brace_expression = expected_ast.add_child(
                if_else_handle,
                Rule::BraceExpression,
                4,
                6,
            );
            // d = b;
            {
                let brace_statements_handle = expected_ast.add_child(
                    brace_expression,
                    Rule::BraceStatements,
                    5,
                    4,
                );
                add_assignment_statement(
                    &mut expected_ast,
                    brace_statements_handle,
                    Token::Symbol("d".to_owned()),
                    5,
                    Token::Symbol("b".to_owned()),
                );
            }

            // no ending expression
            add_terminal_expression(
                &mut expected_ast,
                brace_expression,
                None,
                9,
                0,
            );
        }

        // else
        {
            let expression_handle = expected_ast.add_child(
                if_else_handle,
                Rule::Expression,
                11,
                17,
            );

            let if_else_handle =
                expected_ast.add_child(expression_handle, Rule::IfElse, 11, 17);

            // a == c
            {
                let condition_expression_handle = expected_ast.add_child(
                    if_else_handle,
                    Rule::Expression,
                    12,
                    3,
                );
                add_comparison_tree(
                    &mut expected_ast,
                    condition_expression_handle,
                    Token::Symbol("a".to_owned()),
                    12,
                    Token::Symbol("c".to_owned()),
                );
            }

            // { d = c; }
            {
                let brace_expression = expected_ast.add_child(
                    if_else_handle,
                    Rule::BraceExpression,
                    15,
                    6,
                );
                // d = c;
                {
                    let brace_statements_handle = expected_ast.add_child(
                        brace_expression,
                        Rule::BraceStatements,
                        16,
                        4,
                    );
                    add_assignment_statement(
                        &mut expected_ast,
                        brace_statements_handle,
                        Token::Symbol("d".to_owned()),
                        16,
                        Token::Symbol("c".to_owned()),
                    );
                }

                // no ending expression
                add_terminal_expression(
                    &mut expected_ast,
                    brace_expression,
                    None,
                    20,
                    0,
                );
            }

            // else
            {
                let expression_handle = expected_ast.add_child(
                    if_else_handle,
                    Rule::Expression,
                    22,
                    6,
                );
                let brace_expression_handle = expected_ast.add_child(
                    expression_handle,
                    Rule::BraceExpression,
                    22,
                    6,
                );

                // statements
                let brace_statements_handle = expected_ast.add_child(
                    brace_expression_handle,
                    Rule::BraceStatements,
                    23,
                    4,
                );
                add_assignment_statement(
                    &mut expected_ast,
                    brace_statements_handle,
                    Token::Symbol("d".to_owned()),
                    23,
                    Token::Symbol("e".to_owned()),
                );
                // expression
                add_terminal_expression(
                    &mut expected_ast,
                    brace_expression_handle,
                    None,
                    27,
                    0,
                );
            }
        }
        expected_ast
    };

    check_ast_equal(&ast, &expected_ast);
}

#[test]
fn assign_if_else_if() {
    let tokens = tokenize(
        "
        {
            d = if a == b {
                b
            } else if a == c {
                c
            } else {
                e
            };
        }
    ",
    )
    .expect("Unexpected tokenize error");
    let ast = parse(&tokens).expect("Unexpected parse error");

    let expected_ast = {
        let mut expected_ast = Ast::new();
        let root_handle = expected_ast.add_root(Rule::Expression, 0, 24);
        let brace_expression_handle =
            expected_ast.add_child(root_handle, Rule::BraceExpression, 0, 24);
        let statements_handle = expected_ast.add_child(
            brace_expression_handle,
            Rule::BraceStatements,
            1,
            22,
        );

        let statement_handle =
            expected_ast.add_child(statements_handle, Rule::Statement, 1, 22);

        // Statement LHS
        add_terminal_expression(
            &mut expected_ast,
            statement_handle,
            Some(Token::Symbol("d".to_owned())),
            1,
            1,
        );

        // Statement RHS
        {
            let expression_handle = expected_ast.add_child(
                statement_handle,
                Rule::Expression,
                3,
                19,
            );
            let if_else_handle =
                expected_ast.add_child(expression_handle, Rule::IfElse, 3, 19);

            // a == b
            {
                let condition_expression_handle = expected_ast.add_child(
                    if_else_handle,
                    Rule::Expression,
                    4,
                    3,
                );
                add_comparison_tree(
                    &mut expected_ast,
                    condition_expression_handle,
                    Token::Symbol("a".to_owned()),
                    4,
                    Token::Symbol("b".to_owned()),
                );
            }
            // { b }
            {
                let brace_expression = expected_ast.add_child(
                    if_else_handle,
                    Rule::BraceExpression,
                    7,
                    3,
                );

                // b
                add_terminal_expression(
                    &mut expected_ast,
                    brace_expression,
                    Some(Token::Symbol("b".to_owned())),
                    8,
                    1,
                );
            }

            // else
            {
                let expression_handle = expected_ast.add_child(
                    if_else_handle,
                    Rule::Expression,
                    11,
                    11,
                );

                let if_else_handle = expected_ast.add_child(
                    expression_handle,
                    Rule::IfElse,
                    11,
                    11,
                );

                // a == c
                {
                    let condition_expression_handle = expected_ast.add_child(
                        if_else_handle,
                        Rule::Expression,
                        12,
                        3,
                    );
                    add_comparison_tree(
                        &mut expected_ast,
                        condition_expression_handle,
                        Token::Symbol("a".to_owned()),
                        12,
                        Token::Symbol("c".to_owned()),
                    );
                }

                // { c }
                {
                    let brace_expression = expected_ast.add_child(
                        if_else_handle,
                        Rule::BraceExpression,
                        15,
                        3,
                    );

                    // c
                    add_terminal_expression(
                        &mut expected_ast,
                        brace_expression,
                        Some(Token::Symbol("c".to_owned())),
                        16,
                        1,
                    );
                }

                // else
                {
                    let expression_handle = expected_ast.add_child(
                        if_else_handle,
                        Rule::Expression,
                        19,
                        3,
                    );
                    let brace_expression = expected_ast.add_child(
                        expression_handle,
                        Rule::BraceExpression,
                        19,
                        3,
                    );

                    // e
                    add_terminal_expression(
                        &mut expected_ast,
                        brace_expression,
                        Some(Token::Symbol("e".to_owned())),
                        20,
                        1,
                    );
                }
            }
        }

        // no terminal expression
        add_terminal_expression(
            &mut expected_ast,
            brace_expression_handle,
            None,
            23,
            0,
        );

        expected_ast
    };

    check_ast_equal(&ast, &expected_ast);
}

/// Adds a for loop to the ast that matches the following syntax
/// for (a = 0; a < 10; a = a + 1;)
fn add_for_loop_declaration(
    ast: &mut Ast,
    parent_handle: AstNodeHandle,
    for_loop_start: usize,
    brace_expression_len: usize,
) -> (AstNodeHandle, usize) {
    let declaration_len = 17;
    let for_handle = ast.add_child(
        parent_handle,
        Rule::ForLoop,
        for_loop_start,
        declaration_len + brace_expression_len,
    );

    // init statement
    add_assignment_statement(
        ast,
        for_handle,
        Token::Symbol("a".to_owned()),
        for_loop_start + 2,
        Token::IntLiteral(0),
    );

    // condition statement
    {
        let condition_statement_start = for_loop_start + 6;
        let condition_statement = ast.add_child(
            for_handle,
            Rule::Statement,
            condition_statement_start,
            4,
        );
        let condition_expression = ast.add_child(
            condition_statement,
            Rule::Expression,
            condition_statement_start,
            3,
        );
        let comparison_handle = ast.add_child_with_data(
            condition_expression,
            Rule::Comparison,
            Some(Token::LessThan),
            condition_statement_start,
            3,
        );

        ast.add_terminal_child(
            comparison_handle,
            Some(Token::Symbol("a".to_owned())),
            condition_statement_start,
            1,
        );

        // terminal side
        ast.add_terminal_child(
            comparison_handle,
            Some(Token::IntLiteral(10)),
            condition_statement_start + 2,
            1,
        );
    }
    // increment
    {
        let increment_statement_start = for_loop_start + 10;
        let statement_handle = ast.add_child(
            for_handle,
            Rule::Statement,
            increment_statement_start,
            6,
        );

        // lhs
        add_terminal_expression(
            ast,
            statement_handle,
            Some(Token::Symbol("a".to_owned())),
            increment_statement_start,
            1,
        );

        // rhs
        {
            let expression_handle = ast.add_child(
                statement_handle,
                Rule::Expression,
                increment_statement_start + 2,
                3,
            );
            add_expected_add_child(
                ast,
                expression_handle,
                Token::Symbol("a".to_owned()),
                increment_statement_start + 2,
                Token::IntLiteral(1),
            )
        }
    }

    (for_handle, declaration_len)
}

/// Uses add_for_loop_declaration to make a basic for loop and has the following brace expression
/// {
///     b = 2 * b;
/// }
fn add_basic_for_loop(
    ast: &mut Ast,
    parent_handle: AstNodeHandle,
    for_loop_start: usize,
) -> AstNodeHandle {
    let brace_expression_len = 8;
    let (for_handle, declaration_len) = add_for_loop_declaration(
        ast,
        parent_handle,
        for_loop_start,
        brace_expression_len,
    );

    // brace_expression
    {
        let brace_expression_start = for_loop_start + declaration_len;
        let brace_expression = ast.add_child(
            for_handle,
            Rule::BraceExpression,
            brace_expression_start,
            brace_expression_len,
        );
        // brace statements
        {
            let statements_start = brace_expression_start + 1;
            let brace_statements = ast.add_child(
                brace_expression,
                Rule::BraceStatements,
                statements_start,
                6,
            );
            let statement_handle = ast.add_child(
                brace_statements,
                Rule::Statement,
                statements_start,
                6,
            );

            // statement lhs: assignment
            add_terminal_expression(
                ast,
                statement_handle,
                Some(Token::Symbol("b".to_owned())),
                statements_start,
                1,
            );

            // statement rhs: expression
            {
                let expression_start = statements_start + 2;
                let rhs_expression = ast.add_child(
                    statement_handle,
                    Rule::Expression,
                    expression_start,
                    3,
                );
                add_expected_mult_child(
                    ast,
                    rhs_expression,
                    Token::IntLiteral(2),
                    expression_start,
                    Token::Symbol("b".to_owned()),
                );
            }
        }

        // expression
        add_terminal_expression(
            ast,
            brace_expression,
            None,
            brace_expression_start + brace_expression_len - 1,
            0,
        );
    }

    for_handle
}

#[test]
fn for_loop() {
    let tokens = tokenize(
        "
        for (a = 0; a < 10; a = a + 1;) {
            b = 2 * b;
        }
    ",
    )
    .expect("Unexpected tokenize error");
    let ast = parse(&tokens).expect("Unexpected parse error");

    let expected_ast = {
        let mut expected_ast = Ast::new();
        let root_handle = expected_ast.add_root(Rule::Expression, 0, 25);
        add_basic_for_loop(&mut expected_ast, root_handle, 0);

        expected_ast
    };
    check_ast_equal(&ast, &expected_ast);
}

#[test]
fn for_loop_no_statements() {
    let tokens = tokenize(
        "for () {
            b = 2 * b;
        }",
    )
    .expect("Unexpected tokenize error");
    match parse(&tokens) {
        Ok(_) => assert!(false),
        Err(errors) => {
            assert!(errors.len() == 1);
            let error = errors.get(0).expect("Unexpected get error");
            assert!(error.start_line == 1);
            assert!(error.end_line == 1);
        }
    };
}

#[test]
fn for_loop_one_statement() {
    let tokens = tokenize(
        "for (a = 0;) {
            b = 2 * b;
        }",
    )
    .expect("Unexpected tokenize error");
    match parse(&tokens) {
        Ok(_) => assert!(false),
        Err(errors) => {
            assert!(errors.len() == 1);
            let error = errors.get(0).expect("Unexpected get error");
            assert!(error.start_line == 1);
            assert!(error.end_line == 1);
        }
    };
}

#[test]
fn for_loop_two_statements() {
    let tokens = tokenize(
        "for (a = 0; a < 10) {
            b = 2 * b;
        }",
    )
    .expect("Unexpected tokenize error");
    match parse(&tokens) {
        Ok(_) => assert!(false),
        Err(errors) => {
            assert!(errors.len() == 1);
            let error = errors.get(0).expect("Unexpected get error");
            assert!(error.start_line == 1);
            assert!(error.end_line == 1);
        }
    };
}

#[test]
fn for_loop_no_init() {
    let tokens = tokenize("for (; a < 10; a = a + 1;) {}")
        .expect("Unexpected tokenize error");
    let ast = parse(&tokens).expect("Unexpected parse error");

    let expected_ast = {
        let mut expected_ast = Ast::new();
        let root_handle = expected_ast.add_root(Rule::Expression, 0, 16);

        let for_handle =
            expected_ast.add_child(root_handle, Rule::ForLoop, 0, 16);

        // init statement
        {
            let statement_handle =
                expected_ast.add_child(for_handle, Rule::Statement, 2, 1);
            add_terminal_expression(
                &mut expected_ast,
                statement_handle,
                None,
                2,
                0,
            );
        }

        // condition statement
        {
            let condition_statement =
                expected_ast.add_child(for_handle, Rule::Statement, 3, 4);
            let condition_expression = expected_ast.add_child(
                condition_statement,
                Rule::Expression,
                3,
                3,
            );
            let comparison_handle = expected_ast.add_child_with_data(
                condition_expression,
                Rule::Comparison,
                Some(Token::LessThan),
                3,
                3,
            );

            expected_ast.add_terminal_child(
                comparison_handle,
                Some(Token::Symbol("a".to_owned())),
                3,
                1,
            );

            // terminal side
            expected_ast.add_terminal_child(
                comparison_handle,
                Some(Token::IntLiteral(10)),
                5,
                1,
            );
        }
        // increment
        {
            let statement_handle =
                expected_ast.add_child(for_handle, Rule::Statement, 7, 6);

            // lhs
            add_terminal_expression(
                &mut expected_ast,
                statement_handle,
                Some(Token::Symbol("a".to_owned())),
                7,
                1,
            );

            // rhs
            {
                let expression_handle = expected_ast.add_child(
                    statement_handle,
                    Rule::Expression,
                    9,
                    3,
                );
                add_expected_add_child(
                    &mut expected_ast,
                    expression_handle,
                    Token::Symbol("a".to_owned()),
                    9,
                    Token::IntLiteral(1),
                )
            }
        }

        // brace_expression
        {
            let brace_expression = expected_ast.add_child(
                for_handle,
                Rule::BraceExpression,
                14,
                2,
            );

            // expression
            add_terminal_expression(
                &mut expected_ast,
                brace_expression,
                None,
                15,
                0,
            );
        }

        expected_ast
    };
    check_ast_equal(&ast, &expected_ast);
}

#[test]
fn for_loop_missing_rbrace() {
    let tokens = tokenize(
        "for (a = 0; a < 10; a = a + 1;) {
            b = 2 * b;
    ",
    )
    .expect("Unexpected tokenize error");
    match parse(&tokens) {
        Ok(_) => assert!(false),
        Err(errors) => {
            assert_eq!(errors.len(), 1);
            let error = errors.get(0).expect("Unexpected missing error");
            assert_eq!(error.start_line, 1);
            assert_eq!(error.end_line, 2);
        }
    };
}

#[test]
fn nested_assignment_in_brace_expressions() {
    let tokens = tokenize("{{a = 0;}; a}").expect("Unexpected tokenize error");
    let ast = parse(&tokens).expect("Unexpected parse error");

    let expected_ast = {
        let mut expected_ast = Ast::new();
        let root_handle = expected_ast.add_root(Rule::Expression, 0, 10);

        let brace_expression =
            expected_ast.add_child(root_handle, Rule::BraceExpression, 0, 10);

        // {a = 0;};
        {
            let brace_statements = expected_ast.add_child(
                brace_expression,
                Rule::BraceStatements,
                1,
                7,
            );
            let statement =
                expected_ast.add_child(brace_statements, Rule::Statement, 1, 7);
            let expression =
                expected_ast.add_child(statement, Rule::Expression, 1, 6);
            let brace_expression =
                expected_ast.add_child(expression, Rule::BraceExpression, 1, 6);

            // a = 0;
            {
                let brace_statements = expected_ast.add_child(
                    brace_expression,
                    Rule::BraceStatements,
                    2,
                    4,
                );

                add_assignment_statement(
                    &mut expected_ast,
                    brace_statements,
                    Token::Symbol("a".to_owned()),
                    2,
                    Token::IntLiteral(0),
                );
            }
            // ending expression
            add_terminal_expression(
                &mut expected_ast,
                brace_expression,
                None,
                6,
                0,
            );
        }

        // ending expression
        add_terminal_expression(
            &mut expected_ast,
            brace_expression,
            Some(Token::Symbol("a".to_owned())),
            8,
            1,
        );

        expected_ast
    };
    check_ast_equal(&ast, &expected_ast);
}

#[test]
fn for_loop_brace() {
    let tokens = tokenize(
        "
        for (a = 0; a < 10; a = {(a + 1)};) {
            b = 2 * b;
        }
    ",
    )
    .expect("Unexpected tokenize error");
    let ast = parse(&tokens).expect("Unexpected parse error");

    let expected_ast = {
        let mut expected_ast = Ast::new();
        let root_handle = expected_ast.add_root(Rule::Expression, 0, 29);
        let for_handle =
            expected_ast.add_child(root_handle, Rule::ForLoop, 0, 29);

        // init statement
        add_assignment_statement(
            &mut expected_ast,
            for_handle,
            Token::Symbol("a".to_owned()),
            2,
            Token::IntLiteral(0),
        );

        // condition statement
        {
            let condition_statement =
                expected_ast.add_child(for_handle, Rule::Statement, 6, 4);
            let condition_expression = expected_ast.add_child(
                condition_statement,
                Rule::Expression,
                6,
                3,
            );
            let comparison_handle = expected_ast.add_child_with_data(
                condition_expression,
                Rule::Comparison,
                Some(Token::LessThan),
                6,
                3,
            );

            expected_ast.add_terminal_child(
                comparison_handle,
                Some(Token::Symbol("a".to_owned())),
                6,
                1,
            );

            // terminal side
            expected_ast.add_terminal_child(
                comparison_handle,
                Some(Token::IntLiteral(10)),
                8,
                1,
            );
        }

        // increment
        {
            let statement_handle =
                expected_ast.add_child(for_handle, Rule::Statement, 10, 10);

            // lhs
            add_terminal_expression(
                &mut expected_ast,
                statement_handle,
                Some(Token::Symbol("a".to_owned())),
                10,
                1,
            );

            // rhs
            {
                let expression_handle = expected_ast.add_child(
                    statement_handle,
                    Rule::Expression,
                    12,
                    7,
                );
                let brace_expression_handle = expected_ast.add_child(
                    expression_handle,
                    Rule::BraceExpression,
                    12,
                    7,
                );
                let end_expression_handle = expected_ast.add_child(
                    brace_expression_handle,
                    Rule::Expression,
                    13,
                    5,
                );
                {
                    let paren_expression_handle = expected_ast.add_child(
                        end_expression_handle,
                        Rule::ParenExpression,
                        13,
                        5,
                    );
                    let inner_expression_handle = expected_ast.add_child(
                        paren_expression_handle,
                        Rule::Expression,
                        14,
                        3,
                    );
                    add_expected_add_child(
                        &mut expected_ast,
                        inner_expression_handle,
                        Token::Symbol("a".to_owned()),
                        14,
                        Token::IntLiteral(1),
                    );
                }
            }
        }

        // brace_expression
        {
            let brace_expression = expected_ast.add_child(
                for_handle,
                Rule::BraceExpression,
                21,
                8,
            );
            // brace statements
            {
                let brace_statements = expected_ast.add_child(
                    brace_expression,
                    Rule::BraceStatements,
                    22,
                    6,
                );
                let statement_handle = expected_ast.add_child(
                    brace_statements,
                    Rule::Statement,
                    22,
                    6,
                );

                // statement lhs: assignment
                add_terminal_expression(
                    &mut expected_ast,
                    statement_handle,
                    Some(Token::Symbol("b".to_owned())),
                    22,
                    1,
                );

                // statement rhs: expression
                {
                    let rhs_expression = expected_ast.add_child(
                        statement_handle,
                        Rule::Expression,
                        24,
                        3,
                    );
                    add_expected_mult_child(
                        &mut expected_ast,
                        rhs_expression,
                        Token::IntLiteral(2),
                        24,
                        Token::Symbol("b".to_owned()),
                    );
                }
            }

            // expression
            add_terminal_expression(
                &mut expected_ast,
                brace_expression,
                None,
                28,
                0,
            );
        }

        expected_ast
    };
    check_ast_equal(&ast, &expected_ast);
}

/// multiple braced statements without an expression
#[test]
fn brace_statements() {
    let tokens = tokenize(
        "
        {
            a = b;
            c = d;
            e = f;
        }",
    )
    .expect("Unexpected tokenize error");
    let ast = parse(&tokens).expect("Unexpected parse error");

    let expected_ast = {
        let mut expected_ast = Ast::new();
        let root_handle = expected_ast.add_root(Rule::Expression, 0, 14);
        let brace_expression_handle =
            expected_ast.add_child(root_handle, Rule::BraceExpression, 0, 14);

        // statements
        {
            let statements_handle = expected_ast.add_child(
                brace_expression_handle,
                Rule::BraceStatements,
                1,
                12,
            );

            // recursive statements
            {
                let statements_handle = expected_ast.add_child(
                    statements_handle,
                    Rule::BraceStatements,
                    1,
                    8,
                );

                // recursive statements
                {
                    let statements_handle = expected_ast.add_child(
                        statements_handle,
                        Rule::BraceStatements,
                        1,
                        4,
                    );

                    // a = b;
                    add_assignment_statement(
                        &mut expected_ast,
                        statements_handle,
                        Token::Symbol("a".to_owned()),
                        1,
                        Token::Symbol("b".to_owned()),
                    );
                }

                // c = d;
                add_assignment_statement(
                    &mut expected_ast,
                    statements_handle,
                    Token::Symbol("c".to_owned()),
                    5,
                    Token::Symbol("d".to_owned()),
                );
            }

            // e = f;
            add_assignment_statement(
                &mut expected_ast,
                statements_handle,
                Token::Symbol("e".to_owned()),
                9,
                Token::Symbol("f".to_owned()),
            );
        }

        // no expression at end of braces
        add_terminal_expression(
            &mut expected_ast,
            brace_expression_handle,
            None,
            13,
            0,
        );

        expected_ast
    };

    check_ast_equal(&ast, &expected_ast);
}

#[test]
fn braced_statements_and_expression() {
    let tokens = tokenize(
        "
        {
            a = b;
            c = d;
            e = f;
            e
        }",
    )
    .expect("Unexpected tokenize error");
    let ast = parse(&tokens).expect("Unexpected parse error");

    let expected_ast = {
        let mut expected_ast = Ast::new();
        let root_handle = expected_ast.add_root(Rule::Expression, 0, 15);
        let brace_expression_handle =
            expected_ast.add_child(root_handle, Rule::BraceExpression, 0, 15);

        // statements
        {
            let statements_handle = expected_ast.add_child(
                brace_expression_handle,
                Rule::BraceStatements,
                1,
                12,
            );

            // recursive statements
            {
                let statements_handle = expected_ast.add_child(
                    statements_handle,
                    Rule::BraceStatements,
                    1,
                    8,
                );

                // recursive statements
                {
                    let statements_handle = expected_ast.add_child(
                        statements_handle,
                        Rule::BraceStatements,
                        1,
                        4,
                    );

                    // a = b;
                    add_assignment_statement(
                        &mut expected_ast,
                        statements_handle,
                        Token::Symbol("a".to_owned()),
                        1,
                        Token::Symbol("b".to_owned()),
                    );
                }

                // c = d;
                add_assignment_statement(
                    &mut expected_ast,
                    statements_handle,
                    Token::Symbol("c".to_owned()),
                    5,
                    Token::Symbol("d".to_owned()),
                );
            }

            // e = f;
            add_assignment_statement(
                &mut expected_ast,
                statements_handle,
                Token::Symbol("e".to_owned()),
                9,
                Token::Symbol("f".to_owned()),
            );
        }

        // expression at the end
        add_terminal_expression(
            &mut expected_ast,
            brace_expression_handle,
            Some(Token::Symbol("e".to_owned())),
            13,
            1,
        );

        expected_ast
    };

    check_ast_equal(&ast, &expected_ast);
}

#[test]
fn braced_statements_and_braced_expression() {
    let tokens = tokenize(
        "
        {
            a = b;
            c = d;
            e = f;
            {
                g = f;
                g
            }
        }",
    )
    .expect("Unexpected tokenize error");
    let ast = parse(&tokens).expect("Unexpected parse error");
    let expected_ast = {
        let mut expected_ast = Ast::new();
        let root_handle = expected_ast.add_root(Rule::Expression, 0, 21);
        let brace_expression_handle =
            expected_ast.add_child(root_handle, Rule::BraceExpression, 0, 21);

        // statements
        {
            let statements_handle = expected_ast.add_child(
                brace_expression_handle,
                Rule::BraceStatements,
                1,
                12,
            );

            // recursive statements
            {
                let statements_handle = expected_ast.add_child(
                    statements_handle,
                    Rule::BraceStatements,
                    1,
                    8,
                );

                // recursive statements
                {
                    let statements_handle = expected_ast.add_child(
                        statements_handle,
                        Rule::BraceStatements,
                        1,
                        4,
                    );

                    // a = b;
                    add_assignment_statement(
                        &mut expected_ast,
                        statements_handle,
                        Token::Symbol("a".to_owned()),
                        1,
                        Token::Symbol("b".to_owned()),
                    );
                }

                // c = d;
                add_assignment_statement(
                    &mut expected_ast,
                    statements_handle,
                    Token::Symbol("c".to_owned()),
                    5,
                    Token::Symbol("d".to_owned()),
                );
            }

            // e = f;
            add_assignment_statement(
                &mut expected_ast,
                statements_handle,
                Token::Symbol("e".to_owned()),
                9,
                Token::Symbol("f".to_owned()),
            );
        }

        // expression at the end
        {
            let expression_handle = expected_ast.add_child(
                brace_expression_handle,
                Rule::Expression,
                13,
                7,
            );
            let brace_expression_handle = expected_ast.add_child(
                expression_handle,
                Rule::BraceExpression,
                13,
                7,
            );

            // statements
            {
                let statements_handle = expected_ast.add_child(
                    brace_expression_handle,
                    Rule::BraceStatements,
                    14,
                    4,
                );

                // g = f;
                add_assignment_statement(
                    &mut expected_ast,
                    statements_handle,
                    Token::Symbol("g".to_owned()),
                    14,
                    Token::Symbol("f".to_owned()),
                );
            }

            // expression at end
            add_terminal_expression(
                &mut expected_ast,
                brace_expression_handle,
                Some(Token::Symbol("g".to_owned())),
                18,
                1,
            );
        }

        expected_ast
    };

    check_ast_equal(&ast, &expected_ast);
}

#[test]
fn unary_expansion() {
    let tokens = tokenize("---1").expect("Unexpected tokenize error");
    let ast = parse(&tokens).expect("Unexpected parse error");
    let expected_ast = {
        let mut expected_ast = Ast::new();
        let root_handle = expected_ast.add_root(Rule::Expression, 0, 4);

        let unary_handle = expected_ast.add_child_with_data(
            root_handle,
            Rule::Unary,
            Some(Token::Minus),
            0,
            4,
        );
        let unary_handle = expected_ast.add_child_with_data(
            unary_handle,
            Rule::Unary,
            Some(Token::Minus),
            1,
            3,
        );
        let unary_handle = expected_ast.add_child_with_data(
            unary_handle,
            Rule::Unary,
            Some(Token::Minus),
            2,
            2,
        );
        expected_ast.add_terminal_child(
            unary_handle,
            Some(Token::IntLiteral(1)),
            3,
            1,
        );

        expected_ast
    };
    check_ast_equal(&ast, &expected_ast);
}

/// Because minus is a special operator (being both a binary and unary operator), it has
/// some special case code for handling it, and thus this test case is important
#[test]
fn add_negative_number_lhs() {
    let tokens = tokenize("-1 + 1").expect("Unexpected tokenize error");
    let ast = parse(&tokens).expect("Unexpected parse error");
    let expected_ast = {
        let mut expected_ast = Ast::new();

        let root_handle = expected_ast.add_root(Rule::Expression, 0, 4);

        let plus_minus_handle = expected_ast.add_child_with_data(
            root_handle,
            Rule::PlusMinus,
            Some(Token::Plus),
            0,
            4,
        );

        // LHS
        {
            let unary_handle = expected_ast.add_child_with_data(
                plus_minus_handle,
                Rule::Unary,
                Some(Token::Minus),
                0,
                2,
            );
            expected_ast.add_terminal_child(
                unary_handle,
                Some(Token::IntLiteral(1)),
                1,
                1,
            );
        }

        // RHS
        expected_ast.add_terminal_child(
            plus_minus_handle,
            Some(Token::IntLiteral(1)),
            3,
            1,
        );

        expected_ast
    };

    check_ast_equal(&ast, &expected_ast);
}

/// Because minus is a special operator (being both a binary and unary operator), it has
/// some special case code for handling it, and thus this test case is important
#[test]
fn add_negative_number_rhs() {
    let tokens = tokenize("1 + -1").expect("Unexpected tokenize error");
    let ast = parse(&tokens).expect("Unexpected parse error");
    let expected_ast = {
        let mut expected_ast = Ast::new();

        let root_handle = expected_ast.add_root(Rule::Expression, 0, 4);

        let plus_minus_handle = expected_ast.add_child_with_data(
            root_handle,
            Rule::PlusMinus,
            Some(Token::Plus),
            0,
            4,
        );

        // LHS
        expected_ast.add_terminal_child(
            plus_minus_handle,
            Some(Token::IntLiteral(1)),
            0,
            1,
        );

        // RHS
        {
            let unary_handle = expected_ast.add_child_with_data(
                plus_minus_handle,
                Rule::Unary,
                Some(Token::Minus),
                2,
                2,
            );
            expected_ast.add_terminal_child(
                unary_handle,
                Some(Token::IntLiteral(1)),
                3,
                1,
            );
        }

        expected_ast
    };

    check_ast_equal(&ast, &expected_ast);
}

/// Because minus is a special operator (being both a binary and unary operator), it has
/// some special case code for handling it, and thus this test case is important
#[test]
fn add_negative_number_lhs_unary_expansion() {
    let tokens = tokenize("--1 + 1").expect("Unexpected tokenize error");
    let ast = parse(&tokens).expect("Unexpected parse error");
    let expected_ast = {
        let mut expected_ast = Ast::new();

        let root_handle = expected_ast.add_root(Rule::Expression, 0, 5);

        let plus_minus_handle = expected_ast.add_child_with_data(
            root_handle,
            Rule::PlusMinus,
            Some(Token::Plus),
            0,
            5,
        );

        // LHS
        {
            let unary_handle = expected_ast.add_child_with_data(
                plus_minus_handle,
                Rule::Unary,
                Some(Token::Minus),
                0,
                3,
            );
            let unary_handle = expected_ast.add_child_with_data(
                unary_handle,
                Rule::Unary,
                Some(Token::Minus),
                1,
                2,
            );
            expected_ast.add_terminal_child(
                unary_handle,
                Some(Token::IntLiteral(1)),
                2,
                1,
            );
        }

        // RHS
        expected_ast.add_terminal_child(
            plus_minus_handle,
            Some(Token::IntLiteral(1)),
            4,
            1,
        );

        expected_ast
    };

    check_ast_equal(&ast, &expected_ast);
}

/// Because minus is a special operator (being both a binary and unary operator), it has
/// some special case code for handling it, and thus this test case is important
#[test]
fn add_negative_number_rhs_unary_expansion() {
    let tokens = tokenize("1 + --1").expect("Unexpected tokenize error");
    let ast = parse(&tokens).expect("Unexpected parse error");
    let expected_ast = {
        let mut expected_ast = Ast::new();

        let root_handle = expected_ast.add_root(Rule::Expression, 0, 5);

        let plus_minus_handle = expected_ast.add_child_with_data(
            root_handle,
            Rule::PlusMinus,
            Some(Token::Plus),
            0,
            5,
        );

        // LHS
        expected_ast.add_terminal_child(
            plus_minus_handle,
            Some(Token::IntLiteral(1)),
            0,
            1,
        );

        // RHS
        {
            let unary_handle = expected_ast.add_child_with_data(
                plus_minus_handle,
                Rule::Unary,
                Some(Token::Minus),
                2,
                3,
            );
            let unary_handle = expected_ast.add_child_with_data(
                unary_handle,
                Rule::Unary,
                Some(Token::Minus),
                3,
                2,
            );
            expected_ast.add_terminal_child(
                unary_handle,
                Some(Token::IntLiteral(1)),
                4,
                1,
            );
        }

        expected_ast
    };

    check_ast_equal(&ast, &expected_ast);
}

/// Constructs expected ast for binary op and assignment composition
fn binary_op_and_assign_expected_ast(
    rule: Rule,
    data: Token,
    expression_start: usize,
) -> Ast {
    let expression_len = 6;
    let mut expected_ast = Ast::new();

    let root = expected_ast.add_root(
        Rule::Expression,
        expression_start,
        expression_len,
    );
    let brace_expression = expected_ast.add_child(
        root,
        Rule::BraceExpression,
        expression_start,
        expression_len,
    );

    let statements_start = expression_start + 1;
    let brace_statements = expected_ast.add_child(
        brace_expression,
        Rule::BraceStatements,
        statements_start,
        4,
    );
    {
        let statement = expected_ast.add_child(
            brace_statements,
            Rule::Statement,
            statements_start,
            4,
        );

        // LHS
        add_terminal_expression(
            &mut expected_ast,
            statement,
            Some(Token::Symbol("a".to_owned())),
            statements_start,
            1,
        );

        // RHS
        let rhs_start = statements_start + 2;
        let rhs = expected_ast.add_child(
            statement,
            Rule::Expression,
            statements_start,
            3,
        );
        let binary_op_node_handle = expected_ast.add_child_with_data(
            rhs,
            rule,
            Some(data),
            statements_start,
            3,
        );
        add_terminal_expression(
            &mut expected_ast,
            binary_op_node_handle,
            Some(Token::Symbol("a".to_owned())),
            statements_start,
            1,
        );
        add_terminal_expression(
            &mut expected_ast,
            binary_op_node_handle,
            Some(Token::Symbol("b".to_owned())),
            rhs_start,
            1,
        );
    }

    add_terminal_expression(
        &mut expected_ast,
        brace_expression,
        None,
        statements_start + 4,
        0,
    );

    expected_ast
}

#[test]
fn binary_op_and_assign() {
    let tokens = tokenize("{a += b;}").expect("Unexpected tokenize error");
    let ast = parse(&tokens).expect("Unexpected parse error");
    let expected_ast =
        binary_op_and_assign_expected_ast(Rule::PlusMinus, Token::Plus, 0);

    check_ast_equal(&ast, &expected_ast);
}

#[test]
fn binary_op_and_assign_minus() {
    let tokens = tokenize("{a -= b;}").expect("Unexpected tokenize error");
    let ast = parse(&tokens).expect("Unexpected parse error");
    let expected_ast =
        binary_op_and_assign_expected_ast(Rule::PlusMinus, Token::Minus, 0);

    check_ast_equal(&ast, &expected_ast);
}

#[test]
fn binary_op_and_assign_times() {
    let tokens = tokenize("{a *= b;}").expect("Unexpected tokenize error");
    let ast = parse(&tokens).expect("Unexpected parse error");
    let expected_ast =
        binary_op_and_assign_expected_ast(Rule::MultDiv, Token::Times, 0);

    check_ast_equal(&ast, &expected_ast);
}

#[test]
fn binary_op_and_assign_div() {
    let tokens = tokenize("{a /= b;}").expect("Unexpected tokenize error");
    let ast = parse(&tokens).expect("Unexpected parse error");
    let expected_ast =
        binary_op_and_assign_expected_ast(Rule::MultDiv, Token::Divide, 0);

    check_ast_equal(&ast, &expected_ast);
}

#[test]
fn binary_op_and_assign_rhs_expression() {
    let tokens = tokenize("{a += b - c;}").expect("Unexpected tokenize error");
    let ast = parse(&tokens).expect("Unexpected parse error");
    let expected_ast = {
        let mut expected_ast = Ast::new();

        let root = expected_ast.add_root(Rule::Expression, 0, 8);
        let brace_expression =
            expected_ast.add_child(root, Rule::BraceExpression, 0, 8);

        let brace_statements = expected_ast.add_child(
            brace_expression,
            Rule::BraceStatements,
            1,
            6,
        );
        {
            let statement =
                expected_ast.add_child(brace_statements, Rule::Statement, 1, 6);

            // LHS
            add_terminal_expression(
                &mut expected_ast,
                statement,
                Some(Token::Symbol("a".to_owned())),
                1,
                1,
            );

            // RHS
            let rhs = expected_ast.add_child(statement, Rule::Expression, 1, 5);
            let plus_minus = expected_ast.add_child_with_data(
                rhs,
                Rule::PlusMinus,
                Some(Token::Plus),
                1,
                5,
            );
            add_terminal_expression(
                &mut expected_ast,
                plus_minus,
                Some(Token::Symbol("a".to_owned())),
                1,
                1,
            );

            // b - c
            {
                let expression =
                    expected_ast.add_child(plus_minus, Rule::Expression, 3, 3);
                let plus_minus = expected_ast.add_child_with_data(
                    expression,
                    Rule::PlusMinus,
                    Some(Token::Minus),
                    3,
                    3,
                );

                // b
                expected_ast.add_terminal_child(
                    plus_minus,
                    Some(Token::Symbol("b".to_owned())),
                    3,
                    1,
                );

                // c
                expected_ast.add_terminal_child(
                    plus_minus,
                    Some(Token::Symbol("c".to_owned())),
                    5,
                    1,
                );
            }
        }

        add_terminal_expression(
            &mut expected_ast,
            brace_expression,
            None,
            7,
            0,
        );

        expected_ast
    };

    check_ast_equal(&ast, &expected_ast);
}

#[test]
fn binary_op_and_assign_no_statement() {
    let tokens = tokenize("a += b").expect("Unexpected tokenize error");
    match parse(&tokens) {
        Ok(_) => {
            assert!(false);
        }
        Err(errors) => {
            assert_eq!(errors.len(), 1);
            let error = errors.get(0).expect("Unexpected missing error");
            assert_eq!(error.start_line, 1);
            assert_eq!(error.end_line, 1);
        }
    }
}

#[test]
fn binary_op_repeat() {
    // binary ops cannot repeat, should be parsing error
    let tokens = tokenize("a +* b").expect("Unexpected tokenize error");
    match parse(&tokens) {
        Ok(_) => {
            assert!(false);
        }
        Err(errors) => {
            assert_eq!(errors.len(), 1);
            let error = errors.get(0).expect("Unexpected missing error");
            assert_eq!(error.start_line, 1);
            assert_eq!(error.end_line, 1);
        }
    }
}

#[test]
fn multiple_errors() {
    let tokens =
        tokenize("{a +* b; a +* b;}").expect("Unexpected tokenize error");
    match parse(&tokens) {
        Ok(_) => {
            assert!(false);
        }
        Err(errors) => {
            assert_eq!(errors.len(), 2);

            let error = errors.get(0).expect("Unexpected missing error");
            assert_eq!(error.start_line, 1);
            assert_eq!(error.end_line, 1);

            let error = errors.get(1).expect("Unexpected missing error");
            assert_eq!(error.start_line, 1);
            assert_eq!(error.end_line, 1);
        }
    }
}

#[test]
fn multiple_errors_valid() {
    // multiple errors as well as valid statements
    let tokens = tokenize("{a = b; a +* b; b +/ c; b = a;}")
        .expect("Unexpected tokenize error");
    match parse(&tokens) {
        Ok(_) => {
            assert!(false);
        }
        Err(errors) => {
            assert_eq!(errors.len(), 2);

            let error = errors.get(0).expect("Unexpected missing error");
            assert_eq!(error.start_line, 1);
            assert_eq!(error.end_line, 1);

            let error = errors.get(1).expect("Unexpected missing error");
            assert_eq!(error.start_line, 1);
            assert_eq!(error.end_line, 1);
        }
    }
}

#[test]
fn statement_lhs_is_expression() {
    let tokens = tokenize("{{a} = 1;}").expect("Unexpected tokenize error");
    let ast = parse(&tokens).expect("Unexpected parse error");
    let expected_ast = {
        let mut expected_ast = Ast::new();
        let root_handle = expected_ast.add_root(Rule::Expression, 0, 8);
        let brace_expression =
            expected_ast.add_child(root_handle, Rule::BraceExpression, 0, 8);

        // statements
        {
            let brace_statements = expected_ast.add_child(
                brace_expression,
                Rule::BraceStatements,
                1,
                6,
            );
            let statement_handle =
                expected_ast.add_child(brace_statements, Rule::Statement, 1, 6);

            // LHS
            {
                let expression_handle = expected_ast.add_child(
                    statement_handle,
                    Rule::Expression,
                    1,
                    3,
                );
                let brace_expression = expected_ast.add_child(
                    expression_handle,
                    Rule::BraceExpression,
                    1,
                    3,
                );
                let expression_handle = expected_ast.add_child(
                    brace_expression,
                    Rule::Expression,
                    2,
                    1,
                );
                expected_ast.add_terminal_child(
                    expression_handle,
                    Some(Token::Symbol("a".to_owned())),
                    2,
                    1,
                );
            }

            // RHS
            add_terminal_expression(
                &mut expected_ast,
                statement_handle,
                Some(Token::IntLiteral(1)),
                5,
                1,
            );
        }
        // expression
        add_terminal_expression(
            &mut expected_ast,
            brace_expression,
            None,
            7,
            0,
        );
        expected_ast
    };

    check_ast_equal(&ast, &expected_ast);
}

#[test]
fn statement_lhs_is_function() {
    let tokens = tokenize("{fun(a) = 1;}").expect("Unexpected tokenize error");
    let ast = parse(&tokens).expect("Unexpected parse error");
    let expected_ast = {
        let mut expected_ast = Ast::new();
        let root_handle = expected_ast.add_root(Rule::Expression, 0, 9);
        let brace_expression =
            expected_ast.add_child(root_handle, Rule::BraceExpression, 0, 9);

        // statements
        {
            let brace_statements = expected_ast.add_child(
                brace_expression,
                Rule::BraceStatements,
                1,
                7,
            );
            let statement_handle =
                expected_ast.add_child(brace_statements, Rule::Statement, 1, 7);

            // LHS
            {
                let expression_handle = expected_ast.add_child(
                    statement_handle,
                    Rule::Expression,
                    1,
                    4,
                );
                let fun_call_handle = expected_ast.add_child_with_data(
                    expression_handle,
                    Rule::FunctionCall,
                    Some(Token::Symbol("fun".to_owned())),
                    1,
                    4,
                );
                let fun_arg_handle = expected_ast.add_child(
                    fun_call_handle,
                    Rule::FunctionArguments,
                    3,
                    1,
                );
                let expression_handle = expected_ast.add_child(
                    fun_arg_handle,
                    Rule::Expression,
                    3,
                    1,
                );
                expected_ast.add_terminal_child(
                    expression_handle,
                    Some(Token::Symbol("a".to_owned())),
                    3,
                    1,
                );
            }

            // RHS
            add_terminal_expression(
                &mut expected_ast,
                statement_handle,
                Some(Token::IntLiteral(1)),
                6,
                1,
            );
        }
        // expression
        add_terminal_expression(
            &mut expected_ast,
            brace_expression,
            None,
            8,
            0,
        );
        expected_ast
    };

    check_ast_equal(&ast, &expected_ast);
}

#[test]
fn trailing_binary_op() {
    let tokens = tokenize("a +").expect("Unexpected tokenize error");
    match parse(&tokens) {
        Ok(_) => {
            assert!(false)
        }
        Err(errors) => {
            assert_eq!(errors.len(), 1);
            let error = errors.get(0).expect("Unexpected missing error");
            assert_eq!(error.start_line, 1);
            assert_eq!(error.end_line, 1);
        }
    }
}

#[test]
fn trailing_minus() {
    let tokens = tokenize("a -").expect("Unexpected tokenize error");
    match parse(&tokens) {
        Ok(_) => assert!(false),
        Err(errors) => {
            assert_eq!(errors.len(), 1);

            let error = errors.get(0).expect("Unexpected missing error");
            assert_eq!(error.start_line, 1);
            assert_eq!(error.end_line, 1);
        }
    }
}

fn add_function_call_no_arg(
    ast: &mut Ast,
    parent_handle: AstNodeHandle,
    function_name: String,
    start: usize,
) {
    let function_call_handle = ast.add_child_with_data(
        parent_handle,
        Rule::FunctionCall,
        Some(Token::Symbol(function_name)),
        start,
        3,
    );
    let args_handle = ast.add_child(
        function_call_handle,
        Rule::FunctionArguments,
        start + 2,
        0,
    );
    add_terminal_expression(ast, args_handle, None, start + 2, 0);
}

#[test]
fn function_call_no_arguments() {
    let tokens = tokenize("test()").expect("Unexpected tokenize error");
    let ast = parse(&tokens).expect("Unexpected parse error");
    let expected_ast = {
        let mut expected_ast = Ast::new();
        let root_handle = expected_ast.add_root(Rule::Expression, 0, 3);

        add_function_call_no_arg(
            &mut expected_ast,
            root_handle,
            "test".to_owned(),
            0,
        );
        expected_ast
    };
    check_ast_equal(&ast, &expected_ast);
}

/// Creates the expected tree a single terminal argument (e.g. symbol or number, not an expression) function.
fn add_function_call_one_arg(
    ast: &mut Ast,
    parent_handle: AstNodeHandle,
    function_name: String,
    arg_name: String,
    start: usize,
    expected_len: usize,
) {
    let function_call_handle = ast.add_child_with_data(
        parent_handle,
        Rule::FunctionCall,
        Some(Token::Symbol(function_name)),
        start,
        expected_len,
    );
    let args_handle = ast.add_child(
        function_call_handle,
        Rule::FunctionArguments,
        start + 2,
        expected_len - 3,
    );
    add_terminal_expression(
        ast,
        args_handle,
        Some(Token::Symbol(arg_name)),
        start + 2,
        1,
    );
}

#[test]
fn function_call_one_argument() {
    let tokens = tokenize("test(me)").expect("Unexpected tokenize error");
    let ast = parse(&tokens).expect("Unexpected parse error");

    let expected_ast = {
        let mut expected_ast = Ast::new();
        let root_handle = expected_ast.add_root(Rule::Expression, 0, 4);
        add_function_call_one_arg(
            &mut expected_ast,
            root_handle,
            "test".to_owned(),
            "me".to_owned(),
            0,
            4,
        );

        expected_ast
    };
    check_ast_equal(&ast, &expected_ast);
}

#[test]
fn function_call_two_arguments() {
    let tokens =
        tokenize("test(me, please)").expect("Unexpected tokenize error");
    let ast = parse(&tokens).expect("Unexpected parse error");
    let expected_ast = {
        let mut expected_ast = Ast::new();
        let root_handle = expected_ast.add_root(Rule::Expression, 0, 6);
        let function_call_handle = expected_ast.add_child_with_data(
            root_handle,
            Rule::FunctionCall,
            Some(Token::Symbol("test".to_owned())),
            0,
            6,
        );
        let args_handle = expected_ast.add_child(
            function_call_handle,
            Rule::FunctionArguments,
            2,
            3,
        );

        // "me" argument
        {
            // recursive arg
            let args_handle = expected_ast.add_child(
                args_handle,
                Rule::FunctionArguments,
                2,
                2,
            );
            add_terminal_expression(
                &mut expected_ast,
                args_handle,
                Some(Token::Symbol("me".to_owned())),
                2,
                1,
            );
        }

        // "please" argument
        add_terminal_expression(
            &mut expected_ast,
            args_handle,
            Some(Token::Symbol("please".to_owned())),
            4,
            1,
        );

        expected_ast
    };
    check_ast_equal(&ast, &expected_ast);
}

/// Creates the expected tree a simple three argument function.
/// Factored out b/c we have a trailing comma and non-trailing comma test.
fn add_function_call_multiple_arguments(
    ast: &mut Ast,
    parent_handle: AstNodeHandle,
    function_name: String,
    arg1_name: String,
    arg2_name: String,
    arg3_name: String,
    start: usize,
    len: usize,
) {
    let function_call_handle = ast.add_child_with_data(
        parent_handle,
        Rule::FunctionCall,
        Some(Token::Symbol(function_name)),
        start,
        len,
    );

    let args_handle = ast.add_child(
        function_call_handle,
        Rule::FunctionArguments,
        start + 2,
        len - 3, // don't include parens or function name
    );

    // arg 1 and 2
    {
        // recursive arg
        let args_handle =
            ast.add_child(args_handle, Rule::FunctionArguments, start + 2, 4);

        {
            let args_handle = ast.add_child(
                args_handle,
                Rule::FunctionArguments,
                start + 2,
                2,
            );
            add_terminal_expression(
                ast,
                args_handle,
                Some(Token::Symbol(arg1_name)),
                start + 2,
                1,
            );
        }
        add_terminal_expression(
            ast,
            args_handle,
            Some(Token::Symbol(arg2_name)),
            start + 4,
            1,
        );
    }

    // arg 3
    add_terminal_expression(
        ast,
        args_handle,
        Some(Token::Symbol(arg3_name)),
        start + 6,
        1,
    );
}

#[test]
fn function_call_multiple_arguments() {
    let tokens = tokenize("test(me, please, thanks)")
        .expect("Unexpected tokenize error");
    let ast = parse(&tokens).expect("Unexpected parse error");
    let expected_ast = {
        let mut expected_ast = Ast::new();
        let expected_len = 8;

        let root_handle =
            expected_ast.add_root(Rule::Expression, 0, expected_len);
        add_function_call_multiple_arguments(
            &mut expected_ast,
            root_handle,
            "test".to_owned(),
            "me".to_owned(),
            "please".to_owned(),
            "thanks".to_owned(),
            0,
            expected_len,
        );

        expected_ast
    };
    check_ast_equal(&ast, &expected_ast);
}

#[test]
fn function_call_trailing_comma_single_arg() {
    let tokens = tokenize("test(me,)").expect("Unexpected tokenize error");
    let ast = parse(&tokens).expect("Unexpected parse error");
    let expected_ast = {
        let mut expected_ast = Ast::new();
        let root_handle = expected_ast.add_root(Rule::Expression, 0, 5);
        add_function_call_one_arg(
            &mut expected_ast,
            root_handle,
            "test".to_owned(),
            "me".to_owned(),
            0,
            5,
        );
        expected_ast
    };
    check_ast_equal(&ast, &expected_ast);
}

#[test]
fn function_call_trailing_comma() {
    let tokens = tokenize("test(me, please, thanks,)")
        .expect("Unexpected tokenize error");
    let ast = parse(&tokens).expect("Unexpected parse error");
    let expected_ast = {
        let mut expected_ast = Ast::new();
        let expected_len = 9;
        let root_handle =
            expected_ast.add_root(Rule::Expression, 0, expected_len);
        add_function_call_multiple_arguments(
            &mut expected_ast,
            root_handle,
            "test".to_owned(),
            "me".to_owned(),
            "please".to_owned(),
            "thanks".to_owned(),
            0,
            expected_len,
        );
        expected_ast
    };

    check_ast_equal(&ast, &expected_ast);
}

#[test]
fn function_calls_nested() {
    let tokens = tokenize("test(first_inner(a, b, c,), second_inner(d(e),))")
        .expect("Unexpected tokenize error");

    let ast = parse(&tokens).expect("Unexpected parse error");
    let expected_ast = {
        let mut expected_ast = Ast::new();
        let root_handle = expected_ast.add_root(Rule::Expression, 0, 21);
        let function_call_handle = expected_ast.add_child_with_data(
            root_handle,
            Rule::FunctionCall,
            Some(Token::Symbol("test".to_owned())),
            0,
            21,
        );
        let args_handle = expected_ast.add_child(
            function_call_handle,
            Rule::FunctionArguments,
            2,
            18,
        );

        // arg 1
        {
            let args_handle = expected_ast.add_child(
                args_handle,
                Rule::FunctionArguments,
                2,
                10,
            );
            let expression_handle =
                expected_ast.add_child(args_handle, Rule::Expression, 2, 9);
            add_function_call_multiple_arguments(
                &mut expected_ast,
                expression_handle,
                "first_inner".to_owned(),
                "a".to_owned(),
                "b".to_owned(),
                "c".to_owned(),
                2,
                9,
            );
        }

        // arg 2
        {
            let expression_handle =
                expected_ast.add_child(args_handle, Rule::Expression, 12, 8);
            let function_call_handle = expected_ast.add_child_with_data(
                expression_handle,
                Rule::FunctionCall,
                Some(Token::Symbol("second_inner".to_owned())),
                12,
                8,
            );
            let args_handle = expected_ast.add_child(
                function_call_handle,
                Rule::FunctionArguments,
                14,
                5,
            );
            let expression_handle =
                expected_ast.add_child(args_handle, Rule::Expression, 14, 4);
            add_function_call_one_arg(
                &mut expected_ast,
                expression_handle,
                "d".to_owned(),
                "e".to_owned(),
                14,
                4,
            );
        }
        expected_ast
    };
    check_ast_equal(&ast, &expected_ast);
}

#[test]
fn function_call_argument_expression() {
    let tokens =
        tokenize("test(1 + 2, please,)").expect("Unexpected tokenize error");
    let ast = parse(&tokens).expect("Unexpected parse error");
    let expected_ast = {
        let mut expected_ast = Ast::new();
        let root_handle = expected_ast.add_root(Rule::Expression, 0, 9);
        let function_call_handle = expected_ast.add_child_with_data(
            root_handle,
            Rule::FunctionCall,
            Some(Token::Symbol("test".to_owned())),
            0,
            9,
        );
        let args_handle = expected_ast.add_child(
            function_call_handle,
            Rule::FunctionArguments,
            2,
            6,
        );

        // 1 + 2 argument
        {
            // recursive arg
            let args_handle = expected_ast.add_child(
                args_handle,
                Rule::FunctionArguments,
                2,
                4,
            );

            // 1 + 2
            let expression_handle =
                expected_ast.add_child(args_handle, Rule::Expression, 2, 3);
            add_expected_add_child(
                &mut expected_ast,
                expression_handle,
                Token::IntLiteral(1),
                2,
                Token::IntLiteral(2),
            );
        }

        // "please" argument
        add_terminal_expression(
            &mut expected_ast,
            args_handle,
            Some(Token::Symbol("please".to_owned())),
            6,
            1,
        );

        expected_ast
    };
    check_ast_equal(&ast, &expected_ast);
}

#[test]
fn function_call_parens_in_expression() {
    let tokens =
        tokenize("test((1 + 2), please,)").expect("Unexpected tokenize error");
    let ast = parse(&tokens).expect("Unexpected parse error");
    let expected_ast = {
        let mut expected_ast = Ast::new();
        let root_handle = expected_ast.add_root(Rule::Expression, 0, 11);
        let function_call_handle = expected_ast.add_child_with_data(
            root_handle,
            Rule::FunctionCall,
            Some(Token::Symbol("test".to_owned())),
            0,
            11,
        );
        let args_handle = expected_ast.add_child(
            function_call_handle,
            Rule::FunctionArguments,
            2,
            8,
        );

        // (1 + 2) argument
        {
            // recursive arg
            let args_handle = expected_ast.add_child(
                args_handle,
                Rule::FunctionArguments,
                2,
                6,
            );

            // (1 + 2)
            let expression_handle =
                expected_ast.add_child(args_handle, Rule::Expression, 2, 5);
            let paren_expression_handle = expected_ast.add_child(expression_handle, Rule::ParenExpression, 2, 5);
            let expression_handle = expected_ast.add_child(
                paren_expression_handle,
                Rule::Expression,
                3,
                3,
            );
            // 1 + 2
            add_expected_add_child(
                &mut expected_ast,
                expression_handle,
                Token::IntLiteral(1),
                3,
                Token::IntLiteral(2),
            );
        }

        // "please" argument
        add_terminal_expression(
            &mut expected_ast,
            args_handle,
            Some(Token::Symbol("please".to_owned())),
            8,
            1,
        );

        expected_ast
    };
    check_ast_equal(&ast, &expected_ast);
}

#[test]
fn function_call_braced_expression() {
    let tokens =
        tokenize("test({1 + 2}, please,)").expect("Unexpected tokenize error");
    let ast = parse(&tokens).expect("Unexpected parse error");
    let expected_ast = {
        let mut expected_ast = Ast::new();
        let root_handle = expected_ast.add_root(Rule::Expression, 0, 11);
        let function_call_handle = expected_ast.add_child_with_data(
            root_handle,
            Rule::FunctionCall,
            Some(Token::Symbol("test".to_owned())),
            0,
            11,
        );
        let args_handle = expected_ast.add_child(
            function_call_handle,
            Rule::FunctionArguments,
            2,
            8,
        );

        // {1 + 2} argument
        {
            // recursive arg
            let args_handle = expected_ast.add_child(
                args_handle,
                Rule::FunctionArguments,
                2,
                6,
            );

            // {1 + 2}
            let expression_handle =
                expected_ast.add_child(args_handle, Rule::Expression, 2, 5);
            let brace_expression_handle = expected_ast.add_child(
                expression_handle,
                Rule::BraceExpression,
                2,
                5,
            );
            let expression_handle = expected_ast.add_child(
                brace_expression_handle,
                Rule::Expression,
                3,
                3,
            );
            // 1 + 2
            add_expected_add_child(
                &mut expected_ast,
                expression_handle,
                Token::IntLiteral(1),
                3,
                Token::IntLiteral(2),
            );
        }

        // "please" argument
        add_terminal_expression(
            &mut expected_ast,
            args_handle,
            Some(Token::Symbol("please".to_owned())),
            8,
            1,
        );

        expected_ast
    };
    check_ast_equal(&ast, &expected_ast);
}

/// test sequential function calls
#[test]
fn multiple_function_calls() {
    let tokens = tokenize(
        "{
        a = fun_a();
        fun_b(b);
        fun_c(a, b, c,)
    }",
    )
    .expect("Unexpected tokenize error");
    let ast = parse(&tokens).expect("Unexpected parse error");
    let expected_ast = {
        let mut expected_ast = Ast::new();
        let root_handle = expected_ast.add_root(Rule::Expression, 0, 22);
        let brace_expression_handle =
            expected_ast.add_child(root_handle, Rule::BraceExpression, 0, 22);
        let brace_statements_handle = expected_ast.add_child(
            brace_expression_handle,
            Rule::BraceStatements,
            1,
            11,
        );

        // fun_a()
        {
            // recursive call
            let brace_statements_handle: AstNodeHandle = expected_ast
                .add_child(
                    brace_statements_handle,
                    Rule::BraceStatements,
                    1,
                    6,
                );
            let statement_handle = expected_ast.add_child(
                brace_statements_handle,
                Rule::Statement,
                1,
                6,
            );

            // LHS
            add_terminal_expression(
                &mut expected_ast,
                statement_handle,
                Some(Token::Symbol("a".to_owned())),
                1,
                1,
            );

            // RHS
            {
                let expression_handle = expected_ast.add_child(
                    statement_handle,
                    Rule::Expression,
                    3,
                    3,
                );
                add_function_call_no_arg(
                    &mut expected_ast,
                    expression_handle,
                    "fun_a".to_owned(),
                    3,
                );
            }
        }

        // fun_b(b)
        {
            let statement_handle = expected_ast.add_child(
                brace_statements_handle,
                Rule::Statement,
                7,
                5,
            );
            let expression_handle = expected_ast.add_child(
                statement_handle,
                Rule::Expression,
                7,
                4,
            );
            add_function_call_one_arg(
                &mut expected_ast,
                expression_handle,
                "fun_b".to_owned(),
                "b".to_owned(),
                7,
                4,
            );
        }

        // return expression
        {
            let expression_handle = expected_ast.add_child(
                brace_expression_handle,
                Rule::Expression,
                12,
                9,
            );
            add_function_call_multiple_arguments(
                &mut expected_ast,
                expression_handle,
                "fun_c".to_owned(),
                "a".to_owned(),
                "b".to_owned(),
                "c".to_owned(),
                12,
                9,
            );
        }

        expected_ast
    };

    check_ast_equal(&ast, &expected_ast);
}

#[test]
fn function_call_no_arg_comma() {
    // TODO: a warning here may be a good idea
    let tokens = tokenize("test(,)").expect("Unexpected tokenize error");
    let ast = parse(&tokens).expect("Unexpected parse error");
    let expected_ast = {
        let mut expected_ast = Ast::new();
        let root_handle = expected_ast.add_root(Rule::Expression, 0, 4);
        let function_call_handle = expected_ast.add_child_with_data(
            root_handle,
            Rule::FunctionCall,
            Some(Token::Symbol("test".to_owned())),
            0,
            4,
        );

        let function_arguments_handle = expected_ast.add_child(
            function_call_handle,
            Rule::FunctionArguments,
            2,
            1,
        );
        let expression_handle = expected_ast.add_child(
            function_arguments_handle,
            Rule::Expression,
            2,
            0,
        );
        expected_ast.add_terminal_child(expression_handle, None, 2, 0);

        expected_ast
    };
    check_ast_equal(&ast, &expected_ast);
}

#[test]
fn function_def_no_parens() {
    let tokens = tokenize("fn test {}").expect("Unexpected tokenize error");
    match parse(&tokens) {
        Ok(_) => assert!(false),
        Err(errors) => {
            assert_eq!(errors.len(), 1);
            let error = errors.get(0).expect("Unexpected get error failure");
            assert_eq!(error.start_line, 1);
            assert_eq!(error.end_line, 1);
        }
    }
}

#[test]
fn function_def_no_l_paren() {
    let tokens = tokenize("fn test( {}").expect("Unexpected tokenize error");
    match parse(&tokens) {
        Ok(_) => assert!(false),
        Err(errors) => {
            assert_eq!(errors.len(), 1);
            let error = errors.get(0).expect("Unexpected get error failure");
            assert_eq!(error.start_line, 1);
            assert_eq!(error.end_line, 1);
        }
    }
}

#[test]
fn function_def_no_r_paren() {
    let tokens = tokenize("fn test) {}").expect("Unexpected tokenize error");
    match parse(&tokens) {
        Ok(_) => assert!(false),
        Err(errors) => {
            assert_eq!(errors.len(), 1);
            let error = errors.get(0).expect("Unexpected get error failure");
            assert_eq!(error.start_line, 1);
            assert_eq!(error.end_line, 1);
        }
    }
}

#[test]
fn function_def_non_symbol() {
    let tokens = tokenize("fn 15() {}").expect("Unexpected tokenize error");
    match parse(&tokens) {
        Ok(_) => assert!(false),
        Err(errors) => {
            assert_eq!(errors.len(), 1);
            let error = errors.get(0).expect("Unexpected get error failure");
            assert_eq!(error.start_line, 1);
            assert_eq!(error.end_line, 1);
        }
    }
}

#[test]
fn function_def_non_symbol_declaration() {
    let tokens =
        tokenize("fn test(10 a) {}").expect("Unexpected tokenize error");
    match parse(&tokens) {
        Ok(_) => assert!(false),
        Err(errors) => {
            assert_eq!(errors.len(), 1);
            let error = errors.get(0).expect("Unexpected get error failure");
            assert_eq!(error.start_line, 1);
            assert_eq!(error.end_line, 1);
        }
    }
}

#[test]
fn function_def_non_symbol_params() {
    let tokens =
        tokenize("fn test(int32 10) {}").expect("Unexpected tokenize error");
    match parse(&tokens) {
        Ok(_) => assert!(false),
        Err(errors) => {
            assert_eq!(errors.len(), 1);
            let error = errors.get(0).expect("Unexpected get error failure");
            assert_eq!(error.start_line, 1);
            assert_eq!(error.end_line, 1);
        }
    }
}

#[test]
fn function_def_non_symbol_params_trailing_comma() {
    let tokens =
        tokenize("fn test(int32 10,) {}").expect("Unexpected tokenize error");
    match parse(&tokens) {
        Ok(_) => assert!(false),
        Err(errors) => {
            assert_eq!(errors.len(), 1);
            let error = errors.get(0).expect("Unexpected get error failure");
            assert_eq!(error.start_line, 1);
            assert_eq!(error.end_line, 1);
        }
    }
}

#[test]
fn empty_function() {
    let tokens = tokenize("fn test() {}").expect("Unexpected tokenize error");
    let ast = parse(&tokens).expect("Unexpected parse error");

    let expected_ast = {
        let mut expected_ast = Ast::new();

        let root_handle = expected_ast.add_root(Rule::Expression, 0, 6);
        let function_def_handle = expected_ast.add_child_with_data(
            root_handle,
            Rule::FunctionDef,
            Some(Token::Symbol("test".to_owned())),
            0,
            6,
        );

        // no parameters, so this has no children
        expected_ast.add_child(
            function_def_handle,
            Rule::FunctionDefParameters,
            3,
            0,
        );

        // brace expression
        let brace_expression_handle = expected_ast.add_child(
            function_def_handle,
            Rule::BraceExpression,
            4,
            2,
        );
        add_terminal_expression(
            &mut expected_ast,
            brace_expression_handle,
            None,
            5,
            0,
        );

        expected_ast
    };
    check_ast_equal(&ast, &expected_ast);
}

#[test]
fn multiple_function_defs() {
    let tokens = tokenize(
        "
        fn test1(int32 a, int32 b) {
        }

        fn test2(int32 a, int32 b) {
        }

        fn test3(int32 a, int32 b) {
        }
    ",
    )
    .expect("Unexpected tokenize error");
    let ast = parse(&tokens).expect("Unexpected parse error");

    let expected_ast = {
        let param_one_name = "a".to_owned();
        let param_two_name = "b".to_owned();

        let mut expected_ast = Ast::new();

        let root_handle = expected_ast.add_root(Rule::Expression, 0, 33);
        let function_defs_handle =
            expected_ast.add_child(root_handle, Rule::FunctionDefs, 0, 33);

        // recursive call
        {
            let function_defs_handle = expected_ast.add_child(
                function_defs_handle,
                Rule::FunctionDefs,
                0,
                22,
            );

            let test1_handle = add_basic_function_declaration(
                &mut expected_ast,
                function_defs_handle,
                &"test1".to_string(),
                &param_one_name,
                &param_two_name,
                0,
                7,
                2,
            );
            // brace expression
            let brace_expression_handle = expected_ast.add_child(
                test1_handle,
                Rule::BraceExpression,
                9,
                2,
            );
            add_terminal_expression(
                &mut expected_ast,
                brace_expression_handle,
                None,
                10,
                0,
            );

            let test2_handle = add_basic_function_declaration(
                &mut expected_ast,
                function_defs_handle,
                &"test2".to_string(),
                &param_one_name,
                &param_two_name,
                11,
                7,
                2,
            );
            // brace expression
            let brace_expression_handle = expected_ast.add_child(
                test2_handle,
                Rule::BraceExpression,
                20,
                2,
            );
            add_terminal_expression(
                &mut expected_ast,
                brace_expression_handle,
                None,
                21,
                0,
            );
        }

        let test3_handle = add_basic_function_declaration(
            &mut expected_ast,
            function_defs_handle,
            &"test3".to_string(),
            &param_one_name,
            &param_two_name,
            22,
            7,
            2,
        );

        // brace expression
        let brace_expression_handle =
            expected_ast.add_child(test3_handle, Rule::BraceExpression, 31, 2);
        add_terminal_expression(
            &mut expected_ast,
            brace_expression_handle,
            None,
            32,
            0,
        );

        expected_ast
    };
    check_ast_equal(&ast, &expected_ast);
}

#[test]
fn function_with_returns() {
    let tokens = tokenize("fn test() returns int32 {5}")
        .expect("Unexpected tokenize error");
    let ast = parse(&tokens).expect("Unexpected parse error");

    let expected_ast = {
        let mut expected_ast = Ast::new();

        let root_handle = expected_ast.add_root(Rule::Expression, 0, 9);
        let function_def_handle = expected_ast.add_child_with_data(
            root_handle,
            Rule::FunctionDef,
            Some(Token::Symbol("test".to_owned())),
            0,
            9,
        );

        // no parameters, so this has no children
        expected_ast.add_child(
            function_def_handle,
            Rule::FunctionDefParameters,
            3,
            0,
        );

        let returns_data_handle = expected_ast.add_child(
            function_def_handle,
            Rule::ReturnsData,
            4,
            2,
        );
        expected_ast.add_terminal_child(
            returns_data_handle,
            Some(Token::Symbol("int32".to_owned())),
            4,
            2,
        );

        // brace expression
        let brace_expression_handle = expected_ast.add_child(
            function_def_handle,
            Rule::BraceExpression,
            6,
            3,
        );
        let expression_handle = expected_ast.add_child(
            brace_expression_handle,
            Rule::Expression,
            7,
            1,
        );
        expected_ast.add_terminal_child(
            expression_handle,
            Some(Token::IntLiteral(5)),
            7,
            1,
        );

        expected_ast
    };
    check_ast_equal(&ast, &expected_ast);
}

#[test]
fn for_loop_function() {
    let tokens = tokenize(
        "fn test(int32 a, int32 b) {
            for (a = 0; a < 10; a = a + 1;) {
                b = 2 * b;
            };
            b
        }",
    )
    .expect("Unexpected tokenize error");
    let ast = parse(&tokens).expect("Unexpected parse error");

    let expected_ast = {
        let mut expected_ast = Ast::new();

        let function_name = "test".to_owned();
        let param_one = "a".to_owned();
        let param_two = "b".to_owned();

        let root_handle = expected_ast.add_root(Rule::Expression, 0, 38);

        let function_def_handle = add_basic_function_declaration(
            &mut expected_ast,
            root_handle,
            &function_name,
            &param_one,
            &param_two,
            0,
            7,
            29,
        );

        // brace expression
        let brace_expression_handle = expected_ast.add_child(
            function_def_handle,
            Rule::BraceExpression,
            9,
            29,
        );

        // brace statements
        let brace_statements = expected_ast.add_child(
            brace_expression_handle,
            Rule::BraceStatements,
            10,
            26,
        );
        let statement_handle =
            expected_ast.add_child(brace_statements, Rule::Statement, 10, 26);
        let expression_handle =
            expected_ast.add_child(statement_handle, Rule::Expression, 10, 25);
        add_basic_for_loop(&mut expected_ast, expression_handle, 10);

        add_terminal_expression(
            &mut expected_ast,
            brace_expression_handle,
            Some(Token::Symbol("b".to_owned())),
            36,
            1,
        );

        expected_ast
    };
    check_ast_equal(&ast, &expected_ast);
}

#[test]
fn expression_for_loop_function() {
    let tokens = tokenize(
        "fn test(int32 a, int32 b) {
            for (a = 0; a < 10; a = a + 1;) {
                b = 2 * b;
            }
        }",
    )
    .expect("Unexpected tokenize error");
    let ast = parse(&tokens).expect("Unexpected parse error");

    let expected_ast = {
        let mut expected_ast = Ast::new();

        let function_name = "test".to_owned();
        let param_one = "a".to_owned();
        let param_two = "b".to_owned();

        let root_handle = expected_ast.add_root(Rule::Expression, 0, 36);

        let function_def_handle = add_basic_function_declaration(
            &mut expected_ast,
            root_handle,
            &function_name,
            &param_one,
            &param_two,
            0,
            7,
            27,
        );

        // brace expression
        let brace_expression_handle = expected_ast.add_child(
            function_def_handle,
            Rule::BraceExpression,
            9,
            27,
        );

         // trailing expression
         let expression_handle = expected_ast.add_child(
            brace_expression_handle,
            Rule::Expression,
            10,
            25,
        );
        add_basic_for_loop(&mut expected_ast, expression_handle, 10);

        expected_ast
    };
    check_ast_equal(&ast, &expected_ast);
}

#[test]
fn function_definition_no_params() {
    let tokens = tokenize(
        "
        fn test() {
            a + b
        }",
    )
    .expect("Unexpected tokenize error");
    let ast = parse(&tokens).expect("Unexpected parse error");

    let expected_ast = {
        let mut expected_ast = Ast::new();

        let root_handle = expected_ast.add_root(Rule::Expression, 0, 9);
        let function_def_handle = expected_ast.add_child_with_data(
            root_handle,
            Rule::FunctionDef,
            Some(Token::Symbol("test".to_owned())),
            0,
            9,
        );

        // no parameters, so this has no children
        expected_ast.add_child(
            function_def_handle,
            Rule::FunctionDefParameters,
            3,
            0,
        );

        // brace expression
        let brace_expression_handle = expected_ast.add_child(
            function_def_handle,
            Rule::BraceExpression,
            4,
            5,
        );
        let expression_handle = expected_ast.add_child(
            brace_expression_handle,
            Rule::Expression,
            5,
            3,
        );
        add_expected_add_child(
            &mut expected_ast,
            expression_handle,
            Token::Symbol("a".to_owned()),
            5,
            Token::Symbol("b".to_owned()),
        );

        expected_ast
    };
    check_ast_equal(&ast, &expected_ast);
}

/// adds a one param function as the child of the given parent
fn add_one_param_function(
    ast: &mut Ast,
    parent_handle: AstNodeHandle,
    function_name: String,
    param_type: String,
    param_name: String,
    start: usize,
    function_param_len: usize,
    brace_expression_len: usize,
) {
    let function_def_handle = ast.add_child_with_data(
        parent_handle,
        Rule::FunctionDef,
        Some(Token::Symbol(function_name)),
        start,
        function_param_len + brace_expression_len + 2,
    );

    // parameters
    {
        let function_parameters_handle = ast.add_child(
            function_def_handle,
            Rule::FunctionDefParameters,
            start + 3,
            function_param_len - 2,
        );

        let declaration_handle = ast.add_child(
            function_parameters_handle,
            Rule::Declaration,
            start + 3,
            2,
        );
        ast.add_terminal_child(
            declaration_handle,
            Some(Token::Symbol(param_type)),
            start + 3,
            1,
        );
        ast.add_terminal_child(
            declaration_handle,
            Some(Token::Symbol(param_name.clone())),
            start + 4,
            1,
        );
    }

    let brace_start = start + function_param_len + 2;
    let brace_expression_handle = ast.add_child(
        function_def_handle,
        Rule::BraceExpression,
        brace_start,
        brace_expression_len,
    );
    let expression_handle = ast.add_child(
        brace_expression_handle,
        Rule::Expression,
        brace_start + 1,
        brace_expression_len - 2,
    );
    add_expected_add_child(
        ast,
        expression_handle,
        Token::Symbol(param_name.clone()),
        brace_start + 1,
        Token::IntLiteral(1),
    );
}

#[test]
fn function_definition_one_param() {
    let tokens = tokenize(
        "
        fn test(int32 a) {
            a + 1
        }",
    )
    .expect("Unexpected tokenize error");
    let ast = parse(&tokens).expect("Unexpected parse error");

    let expected_ast = {
        let mut expected_ast = Ast::new();

        let root_handle = expected_ast.add_root(Rule::Expression, 0, 11);
        add_one_param_function(
            &mut expected_ast,
            root_handle,
            "test".to_owned(),
            "int32".to_owned(),
            "a".to_owned(),
            0,
            4,
            5,
        );
        expected_ast
    };
    check_ast_equal(&ast, &expected_ast);
}

/// adds a two param function as the child of the given parent
fn add_two_param_function(
    ast: &mut Ast,
    parent_handle: AstNodeHandle,
    function_name: String,
    param1_type: String,
    param1_name: String,
    param2_type: String,
    param2_name: String,
    start: usize,
    param_len: usize,
    brace_expression_len: usize,
) {
    // TODO: I think that param_len should reflect the length of the FunctionDefParameters function, rather than the length with the parens
    let function_def_handle = ast.add_child_with_data(
        parent_handle,
        Rule::FunctionDef,
        Some(Token::Symbol(function_name)),
        start,
        2 + param_len + brace_expression_len,
    );

    // parameters
    {
        let function_parameters_handle = ast.add_child(
            function_def_handle,
            Rule::FunctionDefParameters,
            3,
            param_len - 2,
        );

        // recursive side
        {
            let function_parameters_handle = ast.add_child(
                function_parameters_handle,
                Rule::FunctionDefParameters,
                3,
                3,
            );

            let declaration_handle = ast.add_child(
                function_parameters_handle,
                Rule::Declaration,
                3,
                2,
            );
            ast.add_terminal_child(
                declaration_handle,
                Some(Token::Symbol(param1_type)),
                3,
                1,
            );
            ast.add_terminal_child(
                declaration_handle,
                Some(Token::Symbol(param1_name.clone())),
                4,
                1,
            );
        }

        // non-recursive side
        {
            let declaration_handle = ast.add_child(
                function_parameters_handle,
                Rule::Declaration,
                6,
                2,
            );
            ast.add_terminal_child(
                declaration_handle,
                Some(Token::Symbol(param2_type.clone())),
                6,
                1,
            );
            ast.add_terminal_child(
                declaration_handle,
                Some(Token::Symbol(param2_name.clone())),
                7,
                1,
            );
        }
    }

    let brace_start = start + param_len + 2;
    let brace_expression_handle = ast.add_child(
        function_def_handle,
        Rule::BraceExpression,
        brace_start,
        brace_expression_len,
    );
    let expression_handle = ast.add_child(
        brace_expression_handle,
        Rule::Expression,
        brace_start + 1,
        3,
    );
    add_expected_add_child(
        ast,
        expression_handle,
        Token::Symbol(param1_name.clone()),
        brace_start + 1,
        Token::Symbol(param2_name.clone()),
    );
}
#[test]
fn function_definition_multiple_params() {
    let tokens = tokenize(
        "
        fn test(int32 a, int32 b) {
            a + b
        }",
    )
    .expect("Unexpected tokenize error");
    let ast = parse(&tokens).expect("Unexpected parse error");

    let expected_ast = {
        let mut expected_ast = Ast::new();

        let root_handle = expected_ast.add_root(Rule::Expression, 0, 14);
        add_two_param_function(
            &mut expected_ast,
            root_handle,
            "test".to_owned(),
            "int32".to_owned(),
            "a".to_owned(),
            "int32".to_owned(),
            "b".to_owned(),
            0,
            7,
            5
        );
        expected_ast
    };
    check_ast_equal(&ast, &expected_ast);
}

#[test]
fn function_definition_trailing_comma_one_arg() {
    let tokens = tokenize(
        "
        fn test(int32 a,) {
            a + 1
        }",
    )
    .expect("Unexpected tokenize error");
    let ast = parse(&tokens).expect("Unexpected parse error");

    let expected_ast = {
        let mut expected_ast = Ast::new();

        let root_handle = expected_ast.add_root(Rule::Expression, 0, 12);
        add_one_param_function(
            &mut expected_ast,
            root_handle,
            "test".to_owned(),
            "int32".to_owned(),
            "a".to_owned(),
            0,
            5,
            5,
        );
        expected_ast
    };
    check_ast_equal(&ast, &expected_ast);
}

#[test]
fn function_definition_trailing_comma_multiple_args() {
    let tokens = tokenize(
        "
        fn test(int32 a, float32 b,) {
            a + b
        }",
    )
    .expect("Unexpected tokenize error");
    let ast = parse(&tokens).expect("Unexpected parse error");

    let expected_ast = {
        let mut expected_ast = Ast::new();

        let root_handle = expected_ast.add_root(Rule::Expression, 0, 15);
        add_two_param_function(
            &mut expected_ast,
            root_handle,
            "test".to_owned(),
            "int32".to_owned(),
            "a".to_owned(),
            "float32".to_owned(),
            "b".to_owned(),
            0,
            8,
            5,
        );
        expected_ast
    };
    check_ast_equal(&ast, &expected_ast);
}

#[test]
fn return_statement_missing_semicolon() {
    let tokens = tokenize(
        "
    fn test(int32 a, int32 b,) {
        return a + b
    }",
    )
    .expect("Unexpected tokenize error");
    match parse(&tokens) {
        Ok(_) => {
            assert!(false);
        }
        Err(errors) => {
            assert_eq!(errors.len(), 1);
            let error = errors.get(0).expect("Unexpected missing error");
            assert_eq!(error.start_line, 3);
            assert_eq!(error.end_line, 3);
        }
    }
}

/// Adds the tree to the AST for a function declaration with two arguments
fn add_basic_function_declaration(
    ast: &mut Ast,
    parent_handle: AstNodeHandle,
    function_name: &String,
    param1_name: &String,
    param2_name: &String,
    start: usize,
    param_len: usize,
    brace_expression_len: usize,
) -> AstNodeHandle {
    let function_def_handle = ast.add_child_with_data(
        parent_handle,
        Rule::FunctionDef,
        Some(Token::Symbol(function_name.clone())),
        start,
        2 + param_len + brace_expression_len,
    );

    // parameters
    {
        let function_parameters_handle = ast.add_child(
            function_def_handle,
            Rule::FunctionDefParameters,
            start + 3,
            param_len - 2, // exclude parens
        );

        // recursive side
        let first_param_start = start + 3; // move past fn, symbol, lparen
        {
            let function_parameters_handle = ast.add_child(
                function_parameters_handle,
                Rule::FunctionDefParameters,
                first_param_start,
                3, // 3 includes the comma
            );

            // non-recursive side
            let declaration_handle = ast.add_child(
                function_parameters_handle,
                Rule::Declaration,
                first_param_start,
                2,
            );
            ast.add_terminal_child(
                declaration_handle,
                Some(Token::Symbol("int32".to_owned())),
                first_param_start,
                1,
            );
            ast.add_terminal_child(
                declaration_handle,
                Some(Token::Symbol(param1_name.clone())),
                first_param_start + 1,
                1,
            );
        }

        // non-recursive side
        {
            let second_param_start = first_param_start + 3;
            let declaration_handle = ast.add_child(
                function_parameters_handle,
                Rule::Declaration,
                second_param_start,
                2,
            );
            ast.add_terminal_child(
                declaration_handle,
                Some(Token::Symbol("int32".to_owned())),
                second_param_start,
                1,
            );
            ast.add_terminal_child(
                declaration_handle,
                Some(Token::Symbol(param2_name.clone())),
                second_param_start + 1,
                1,
            );
        }
    }

    function_def_handle
}

#[test]
fn function_definition_return() {
    let tokens = tokenize(
        "
    fn test(int32 a, int32 b,) {
        return a + b;
    }",
    )
    .expect("Unexpected tokenize error");
    let ast = parse(&tokens).expect("Unexpected parse error");
    let expected_ast = {
        let function_name = "test".to_owned();
        let param1_name = "a".to_owned();
        let param2_name = "b".to_owned();

        let mut expected_ast = Ast::new();
        let root_handle = expected_ast.add_root(Rule::Expression, 0, 17);

        let function_def_handle = add_basic_function_declaration(
            &mut expected_ast,
            root_handle,
            &function_name,
            &param1_name,
            &param2_name,
            0,
            8,
            7,
        );

        let brace_expression_handle = expected_ast.add_child(
            function_def_handle,
            Rule::BraceExpression,
            10,
            7,
        );

        // brace statements
        {
            let brace_statements = expected_ast.add_child(
                brace_expression_handle,
                Rule::BraceStatements,
                11,
                5,
            );
            let return_statement = expected_ast.add_child(
                brace_statements,
                Rule::ReturnStatement,
                11,
                5,
            );
            let return_expression = expected_ast.add_child(
                return_statement,
                Rule::Expression,
                12,
                3,
            );
            add_expected_add_child(
                &mut expected_ast,
                return_expression,
                Token::Symbol(param1_name.clone()),
                12,
                Token::Symbol(param2_name.clone()),
            );
        }

        add_terminal_expression(
            &mut expected_ast,
            brace_expression_handle,
            None,
            16,
            0,
        );

        expected_ast
    };

    check_ast_equal(&ast, &expected_ast);
}

#[test]
fn function_definition_multiple_returns() {
    let tokens = tokenize(
        "
    fn test(int32 a, int32 b,) {
        if a > 0 {
            return a;
        } else {
            return b;
        };
    }",
    )
    .expect("Unexpected tokenize error");

    let ast = parse(&tokens).expect("Unexpected parse error");
    let expected_ast = {
        let function_name = "test".to_owned();
        let param1_name = "a".to_owned();
        let param2_name = "b".to_owned();

        let mut expected_ast = Ast::new();
        let root_handle = expected_ast.add_root(Rule::Expression, 0, 28);

        let function_def_handle = add_basic_function_declaration(
            &mut expected_ast,
            root_handle,
            &function_name,
            &param1_name,
            &param2_name,
            0,
            8,
            18,
        );

        let brace_expression_handle = expected_ast.add_child(
            function_def_handle,
            Rule::BraceExpression,
            10,
            18,
        );

        // brace statements
        {
            let brace_statements = expected_ast.add_child(
                brace_expression_handle,
                Rule::BraceStatements,
                11,
                16,
            );

            let statement_handle = expected_ast.add_child(
                brace_statements,
                Rule::Statement,
                11,
                16,
            );
            let expression_handle = expected_ast.add_child(
                statement_handle,
                Rule::Expression,
                11,
                15,
            );
            let if_else_handle =
                expected_ast.add_child(expression_handle, Rule::IfElse, 11, 15);

            // condition
            {
                let condition_expression_handle = expected_ast.add_child(
                    if_else_handle,
                    Rule::Expression,
                    12,
                    3,
                );
                let equality_handle = expected_ast.add_child_with_data(
                    condition_expression_handle,
                    Rule::Comparison,
                    Some(Token::GreaterThan),
                    12,
                    3,
                );

                // LHS
                expected_ast.add_child_with_data(
                    equality_handle,
                    Rule::Terminal,
                    Some(Token::Symbol("a".to_owned())),
                    12,
                    1,
                );

                // RHS
                expected_ast.add_child_with_data(
                    equality_handle,
                    Rule::Terminal,
                    Some(Token::IntLiteral(0)),
                    14,
                    1,
                );
            }

            // if side
            {
                let brace_expression_handle = expected_ast.add_child(
                    if_else_handle,
                    Rule::BraceExpression,
                    15,
                    5,
                );
                let brace_statements_handle = expected_ast.add_child(
                    brace_expression_handle,
                    Rule::BraceStatements,
                    16,
                    3,
                );
                let return_statement_handle = expected_ast.add_child(
                    brace_statements_handle,
                    Rule::ReturnStatement,
                    16,
                    3,
                );
                add_terminal_expression(
                    &mut expected_ast,
                    return_statement_handle,
                    Some(Token::Symbol("a".to_owned())),
                    17,
                    1,
                );

                add_terminal_expression(
                    &mut expected_ast,
                    brace_expression_handle,
                    None,
                    19,
                    0,
                );
            }

            // else side
            {
                let expression_handle = expected_ast.add_child(
                    if_else_handle,
                    Rule::Expression,
                    21,
                    5,
                );
                let brace_expression_handle = expected_ast.add_child(
                    expression_handle,
                    Rule::BraceExpression,
                    21,
                    5,
                );
                let brace_statements_handle = expected_ast.add_child(
                    brace_expression_handle,
                    Rule::BraceStatements,
                    22,
                    3,
                );
                let return_statement_handle = expected_ast.add_child(
                    brace_statements_handle,
                    Rule::ReturnStatement,
                    22,
                    3,
                );
                add_terminal_expression(
                    &mut expected_ast,
                    return_statement_handle,
                    Some(Token::Symbol("b".to_owned())),
                    23,
                    1,
                );

                add_terminal_expression(
                    &mut expected_ast,
                    brace_expression_handle,
                    None,
                    25,
                    0,
                );
            }
        }

        add_terminal_expression(
            &mut expected_ast,
            brace_expression_handle,
            None,
            27,
            0,
        );

        expected_ast
    };

    check_ast_equal(&ast, &expected_ast);
}

#[test]
fn function_definition_if_else() {
    let tokens = tokenize(
        "
    fn test(int32 a, int32 b,) {
        if (a > 0) {
            a
        } else {
            b
        }
    }",
    )
    .expect("Unexpected tokenize error");

    let ast = parse(&tokens).expect("Unexpected parse error");
    let expected_ast = {
        let function_name = "test".to_owned();
        let param1_name = "a".to_owned();
        let param2_name = "b".to_owned();

        let mut expected_ast = Ast::new();
        let root_handle = expected_ast.add_root(Rule::Expression, 0, 25);

        let function_def_handle = add_basic_function_declaration(
            &mut expected_ast,
            root_handle,
            &function_name,
            &param1_name,
            &param2_name,
            0,
            8,
            15,
        );

        let brace_expression_handle = expected_ast.add_child(
            function_def_handle,
            Rule::BraceExpression,
            10,
            15,
        );

        let expression_handle = expected_ast.add_child(
            brace_expression_handle,
            Rule::Expression,
            11,
            13,
        );
        let if_else_handle =
            expected_ast.add_child(expression_handle, Rule::IfElse, 11, 13);

        // condition
        {
            let condition_expression_handle =
                expected_ast.add_child(if_else_handle, Rule::Expression, 12, 5);            
            let condition_expression_handle = expected_ast.add_child(
                condition_expression_handle,
                Rule::ParenExpression,
                12,
                5,
            );
            let condition_expression_handle = expected_ast.add_child(
                condition_expression_handle,
                Rule::Expression,
                13,
                3,
            );

            let equality_handle = expected_ast.add_child_with_data(
                condition_expression_handle,
                Rule::Comparison,
                Some(Token::GreaterThan),
                13,
                3,
            );

            // LHS
            expected_ast.add_child_with_data(
                equality_handle,
                Rule::Terminal,
                Some(Token::Symbol("a".to_owned())),
                13,
                1,
            );

            // RHS
            expected_ast.add_child_with_data(
                equality_handle,
                Rule::Terminal,
                Some(Token::IntLiteral(0)),
                15,
                1,
            );
        }

        // if side
        {
            let brace_expression_handle = expected_ast.add_child(
                if_else_handle,
                Rule::BraceExpression,
                17,
                3,
            );
            add_terminal_expression(
                &mut expected_ast,
                brace_expression_handle,
                Some(Token::Symbol("a".to_owned())),
                18,
                1,
            );
        }

        // else side
        {
            let expression_handle =
                expected_ast.add_child(if_else_handle, Rule::Expression, 21, 3);
            let brace_expression_handle = expected_ast.add_child(
                expression_handle,
                Rule::BraceExpression,
                21,
                3,
            );

            add_terminal_expression(
                &mut expected_ast,
                brace_expression_handle,
                Some(Token::Symbol("b".to_owned())),
                22,
                1,
            );
        }

        expected_ast
    };

    check_ast_equal(&ast, &expected_ast);
}

#[test]
fn function_definition_early_return() {
    let tokens = tokenize(
        "
    fn test(int32 a, int32 b,) {
        if a > 0 {
            return a;
        };

        return b;
    }",
    )
    .expect("Unexpected tokenize error");

    let ast = parse(&tokens).expect("Unexpected parse error");
    let expected_ast =
        {
            let function_name = "test".to_owned();
            let param1_name = "a".to_owned();
            let param2_name = "b".to_owned();

            let mut expected_ast = Ast::new();
            let root_handle = expected_ast.add_root(Rule::Expression, 0, 25);

            let function_def_handle = add_basic_function_declaration(
                &mut expected_ast,
                root_handle,
                &function_name,
                &param1_name,
                &param2_name,
                0,
                8,
                15,
            );

            let brace_expression_handle = expected_ast.add_child(
                function_def_handle,
                Rule::BraceExpression,
                10,
                15,
            );

            // brace statements
            {
                let brace_statements = expected_ast.add_child(
                    brace_expression_handle,
                    Rule::BraceStatements,
                    11,
                    13,
                );

                // first statement
                {
                    let brace_statements = expected_ast.add_child(
                        brace_statements,
                        Rule::BraceStatements,
                        11,
                        10,
                    );
                    let statement_handle = expected_ast.add_child(
                        brace_statements,
                        Rule::Statement,
                        11,
                        10,
                    );
                    let expression_handle = expected_ast.add_child(
                        statement_handle,
                        Rule::Expression,
                        11,
                        9,
                    );
                    let if_else_handle = expected_ast.add_child(
                        expression_handle,
                        Rule::IfElse,
                        11,
                        9,
                    );

                    // condition
                    {
                        let condition_expression_handle = expected_ast
                            .add_child(if_else_handle, Rule::Expression, 12, 3);
                        let equality_handle = expected_ast.add_child_with_data(
                            condition_expression_handle,
                            Rule::Comparison,
                            Some(Token::GreaterThan),
                            12,
                            3,
                        );

                        // LHS
                        expected_ast.add_child_with_data(
                            equality_handle,
                            Rule::Terminal,
                            Some(Token::Symbol("a".to_owned())),
                            12,
                            1,
                        );

                        // RHS
                        expected_ast.add_child_with_data(
                            equality_handle,
                            Rule::Terminal,
                            Some(Token::IntLiteral(0)),
                            14,
                            1,
                        );
                    }

                    // if side
                    {
                        let brace_expression_handle = expected_ast.add_child(
                            if_else_handle,
                            Rule::BraceExpression,
                            15,
                            5,
                        );
                        let brace_statements_handle = expected_ast.add_child(
                            brace_expression_handle,
                            Rule::BraceStatements,
                            16,
                            3,
                        );
                        let return_statement_handle = expected_ast.add_child(
                            brace_statements_handle,
                            Rule::ReturnStatement,
                            16,
                            3,
                        );
                        add_terminal_expression(
                            &mut expected_ast,
                            return_statement_handle,
                            Some(Token::Symbol("a".to_owned())),
                            17,
                            1,
                        );

                        add_terminal_expression(
                            &mut expected_ast,
                            brace_expression_handle,
                            None,
                            19,
                            0,
                        );
                    }
                }

                // second statement
                {
                    let return_statement_handle = expected_ast.add_child(
                        brace_statements,
                        Rule::ReturnStatement,
                        21,
                        3,
                    );
                    add_terminal_expression(
                        &mut expected_ast,
                        return_statement_handle,
                        Some(Token::Symbol("b".to_owned())),
                        22,
                        1,
                    );
                }
            }

            add_terminal_expression(
                &mut expected_ast,
                brace_expression_handle,
                None,
                24,
                0,
            );

            expected_ast
        };

    check_ast_equal(&ast, &expected_ast);
}

#[test]
fn function_definition_final_expression() {
    let tokens = tokenize(
        "
    fn test(int32 a, int32 b,) {
        if a > 0 {
            return a;
        };

        b
    }",
    )
    .expect("Unexpected tokenize error");

    let ast = parse(&tokens).expect("Unexpected parse error");
    let expected_ast =
        {
            let function_name = "test".to_owned();
            let param1_name = "a".to_owned();
            let param2_name = "b".to_owned();

            let mut expected_ast = Ast::new();
            let root_handle = expected_ast.add_root(Rule::Expression, 0, 23);

            let function_def_handle = add_basic_function_declaration(
                &mut expected_ast,
                root_handle,
                &function_name,
                &param1_name,
                &param2_name,
                0,
                8,
                13,
            );

            let brace_expression_handle = expected_ast.add_child(
                function_def_handle,
                Rule::BraceExpression,
                10,
                13,
            );

            // brace statements
            {
                let brace_statements = expected_ast.add_child(
                    brace_expression_handle,
                    Rule::BraceStatements,
                    11,
                    10,
                );

                // first statement
                {
                    let statement_handle = expected_ast.add_child(
                        brace_statements,
                        Rule::Statement,
                        11,
                        10,
                    );
                    let expression_handle = expected_ast.add_child(
                        statement_handle,
                        Rule::Expression,
                        11,
                        9,
                    );
                    let if_else_handle = expected_ast.add_child(
                        expression_handle,
                        Rule::IfElse,
                        11,
                        9,
                    );

                    // condition
                    {
                        let condition_expression_handle = expected_ast
                            .add_child(if_else_handle, Rule::Expression, 12, 3);
                        let equality_handle = expected_ast.add_child_with_data(
                            condition_expression_handle,
                            Rule::Comparison,
                            Some(Token::GreaterThan),
                            12,
                            3,
                        );

                        // LHS
                        expected_ast.add_child_with_data(
                            equality_handle,
                            Rule::Terminal,
                            Some(Token::Symbol("a".to_owned())),
                            12,
                            1,
                        );

                        // RHS
                        expected_ast.add_child_with_data(
                            equality_handle,
                            Rule::Terminal,
                            Some(Token::IntLiteral(0)),
                            14,
                            1,
                        );
                    }

                    // if side
                    {
                        let brace_expression_handle = expected_ast.add_child(
                            if_else_handle,
                            Rule::BraceExpression,
                            15,
                            5,
                        );
                        let brace_statements_handle = expected_ast.add_child(
                            brace_expression_handle,
                            Rule::BraceStatements,
                            16,
                            3,
                        );
                        let return_statement_handle = expected_ast.add_child(
                            brace_statements_handle,
                            Rule::ReturnStatement,
                            16,
                            3,
                        );
                        add_terminal_expression(
                            &mut expected_ast,
                            return_statement_handle,
                            Some(Token::Symbol("a".to_owned())),
                            17,
                            1,
                        );

                        add_terminal_expression(
                            &mut expected_ast,
                            brace_expression_handle,
                            None,
                            19,
                            0,
                        );
                    }
                }
            }

            add_terminal_expression(
                &mut expected_ast,
                brace_expression_handle,
                Some(Token::Symbol("b".to_owned())),
                21,
                1,
            );

            expected_ast
        };

    check_ast_equal(&ast, &expected_ast);
}

/// Should result in a syntax error
#[test]
fn function_definition_early_expression() {
    let tokens = tokenize(
        "
    fn test(int32 a, int32 b,) {
        if (a > 0) {
            a
        }

        return b;
    }",
    )
    .expect("Unexpected tokenize error");

    match parse(&tokens) {
        Ok(_) => assert!(false),
        Err(errors) => {
            assert_eq!(errors.len(), 1);
            let error = errors.get(0).expect("Unexpected missing error");

            assert_eq!(error.start_line, 7);
            assert_eq!(error.end_line, 7);
        }
    };
}

#[test]
fn for_loop_return() {
    let tokens = tokenize(
        "
    fn test(int32 a, int32 b,) {
        for (a = 0; a < 10; a = a + 1;) {
            return b;
        };
    }
    ",
    )
    .expect("Unexpected tokenize error");
    let ast = parse(&tokens).expect("Unexpected parse error");

    let expected_ast = {
        let function_name = "test".to_owned();
        let param1_name = "a".to_owned();
        let param2_name = "b".to_owned();

        let mut expected_ast = Ast::new();

        let root_handle = expected_ast.add_root(Rule::Expression, 0, 35);

        let function_def_handle = add_basic_function_declaration(
            &mut expected_ast,
            root_handle,
            &function_name,
            &param1_name,
            &param2_name,
            0,
            8,
            25,
        );

        let brace_expression_handle = expected_ast.add_child(
            function_def_handle,
            Rule::BraceExpression,
            10,
            25,
        );

        // statements
        {
            let brace_statements = expected_ast.add_child(
                brace_expression_handle,
                Rule::BraceStatements,
                11,
                23,
            );
            let statement_handle = expected_ast.add_child(
                brace_statements,
                Rule::Statement,
                11,
                23,
            );
            let expression_handle = expected_ast.add_child(
                statement_handle,
                Rule::Expression,
                11,
                22,
            );

            let for_loop_start = 11;
            let for_loop_brace_expression_len = 5;
            let (for_handle, declaration_len) = add_for_loop_declaration(
                &mut expected_ast,
                expression_handle,
                for_loop_start,
                for_loop_brace_expression_len,
            );
            // for loop brace_expression
            {
                let brace_start = for_loop_start + declaration_len;
                let brace_expression = expected_ast.add_child(
                    for_handle,
                    Rule::BraceExpression,
                    brace_start,
                    for_loop_brace_expression_len,
                );
                // brace statements
                {
                    let brace_statements = expected_ast.add_child(
                        brace_expression,
                        Rule::BraceStatements,
                        brace_start + 1,
                        3,
                    );
                    let return_statement_handle = expected_ast.add_child(
                        brace_statements,
                        Rule::ReturnStatement,
                        brace_start + 1,
                        3,
                    );

                    add_terminal_expression(
                        &mut expected_ast,
                        return_statement_handle,
                        Some(Token::Symbol("b".to_owned())),
                        brace_start + 2,
                        1,
                    );
                }

                // expression
                add_terminal_expression(
                    &mut expected_ast,
                    brace_expression,
                    None,
                    brace_start + 4,
                    0,
                );
            }
        }

        add_terminal_expression(
            &mut expected_ast,
            brace_expression_handle,
            None,
            34,
            0,
        );
        expected_ast
    };
    check_ast_equal(&ast, &expected_ast);
}

/// should result in a syntax error
#[test]
fn multiple_expressions() {
    let tokens = tokenize(
        "
    fn test(int32 a, int32 b,) {
        {
            a
        }

        {
            b
        }
    }",
    )
    .expect("Unexpected tokenize error");
    match parse(&tokens) {
        Ok(_) => assert!(false),
        Err(errors) => {
            assert_eq!(errors.len(), 1);
            let error = errors.get(0).expect("Unexpected missing error");
            assert_eq!(error.start_line, 4);
            assert_eq!(error.end_line, 8);
        }
    };
}

/// technically valid syntax, but results in dead code
#[test]
fn return_then_expression() {
    let tokens = tokenize(
        "
    fn test(int32 a, int32 b,) {
        return a;
        b
    }",
    )
    .expect("Unexpected tokenize error");

    let ast = parse(&tokens).expect("Unexpected parse error");

    let expected_ast = {
        let function_name = "test".to_owned();
        let param1_name = "a".to_owned();
        let param2_name = "b".to_owned();

        let mut expected_ast = Ast::new();
        let root_handle = expected_ast.add_root(Rule::Expression, 0, 16);

        let function_def_handle = add_basic_function_declaration(
            &mut expected_ast,
            root_handle,
            &function_name,
            &param1_name,
            &param2_name,
            0,
            8,
            6,
        );

        let brace_expression_handle = expected_ast.add_child(
            function_def_handle,
            Rule::BraceExpression,
            10,
            6,
        );

        // brace statements
        {
            let brace_statements = expected_ast.add_child(
                brace_expression_handle,
                Rule::BraceStatements,
                11,
                3,
            );

            // first statement
            {
                let return_statement_handle = expected_ast.add_child(
                    brace_statements,
                    Rule::ReturnStatement,
                    11,
                    3,
                );
                add_terminal_expression(
                    &mut expected_ast,
                    return_statement_handle,
                    Some(Token::Symbol("a".to_owned())),
                    11,
                    1,
                );
            }
        }

        add_terminal_expression(
            &mut expected_ast,
            brace_expression_handle,
            Some(Token::Symbol("b".to_owned())),
            14,
            1,
        );

        expected_ast
    };

    check_ast_equal(&ast, &expected_ast);
}

#[test]
fn missing_for_rparen() {
    let tokens = tokenize(
        "for (a = 0; a < 10; a = a + 1; {
            b = 2 * b;
        }",
    )
    .expect("Unexpected tokenize error");

    match parse(&tokens) {
        Ok(_) => assert!(false),
        Err(errors) => {
            assert_eq!(errors.len(), 1);
            let error = errors.get(0).expect("Missing error");
            assert_eq!(error.start_line, 1);
            assert_eq!(error.end_line, 3);
        }
    };
}

#[test]
fn missing_if_lbrace() {
    let tokens = tokenize(
        "if (a + b) == c
            d = c;
        } else {
            d = e;
        }",
    )
    .expect("Unexpected tokenize error");

    match parse(&tokens) {
        Ok(_) => assert!(false),
        Err(errors) => {
            assert_eq!(errors.len(), 1);
            let error = errors.get(0).expect("Missing error");
            assert_eq!(error.start_line, 1);
            assert_eq!(error.end_line, 3);
        }
    };
}

#[test]
fn missing_if_braces() {
    let tokens = tokenize(
        "if (a)
            d = c;
        else {
            d = e;
        }",
    )
    .expect("Unexpected tokenize error");

    match parse(&tokens) {
        Ok(_) => assert!(false),
        Err(errors) => {
            assert_eq!(errors.len(), 1);
            let error = errors.get(0).expect("Missing error");
            assert_eq!(error.start_line, 1);
            assert_eq!(error.end_line, 3);
        }
    };
}

#[test]
fn missing_if_braces_unbalanced() {
    let tokens = tokenize(
        "if (a) {
            d = c;
        }} else {
            d = e;
        }",
    )
    .expect("Unexpected tokenize error");

    match parse(&tokens) {
        Ok(_) => assert!(false),
        Err(errors) => {
            assert_eq!(errors.len(), 1);
            let error = errors.get(0).expect("Missing error");
            assert_eq!(error.start_line, 1);
            assert_eq!(error.end_line, 3);
        }
    };
}

#[test]
fn parse_data_structure_declaration() {
    let tokens = tokenize(
        "
        struct data_struct {
            int32 field;
        }
    ",
    )
    .expect("Unexpected tokenize failure");
    let ast = parse(&tokens).expect("Unexpected parse error");
    let expected_ast = {
        let mut expected_ast = Ast::new();
        let root_handle = expected_ast.add_root(Rule::Expression, 0, 7);
        let data_structure_handle = expected_ast.add_child_with_data(
            root_handle,
            Rule::DataStructure,
            Some(Token::Symbol("data_struct".to_owned())),
            0,
            7,
        );
        let declaration_statements_handle = expected_ast.add_child(
            data_structure_handle,
            Rule::DeclarationStatements,
            2,
            5,
        );
        let declaration_statement = expected_ast.add_child(
            declaration_statements_handle,
            Rule::Declaration,
            3,
            3,
        );
        expected_ast.add_terminal_child(
            declaration_statement,
            Some(Token::Symbol("int32".to_owned())),
            3,
            1,
        );
        expected_ast.add_terminal_child(
            declaration_statement,
            Some(Token::Symbol("field".to_owned())),
            4,
            1,
        );

        expected_ast
    };

    check_ast_equal(&ast, &expected_ast);
}

#[test]
fn parse_data_struct_multiple_fields() {
    let tokens = tokenize(
        "
        struct data_struct {
            int32 field;
            uint32 field2;
        }
    ",
    )
    .expect("Unexpected tokenize failure");
    let ast = parse(&tokens).expect("Unexpected parse error");
    let expected_ast = {
        let mut expected_ast = Ast::new();
        let root_handle = expected_ast.add_root(Rule::Expression, 0, 10);
        let data_structure_handle = expected_ast.add_child_with_data(
            root_handle,
            Rule::DataStructure,
            Some(Token::Symbol("data_struct".to_owned())),
            0,
            10,
        );
        let declaration_statements_handle = expected_ast.add_child(
            data_structure_handle,
            Rule::DeclarationStatements,
            2,
            8,
        );

        // LHS
        {
            let recursive_handle = expected_ast.add_child(
                declaration_statements_handle,
                Rule::DeclarationStatements,
                3,
                3,
            );
            let declaration_statement = expected_ast.add_child(
                recursive_handle,
                Rule::Declaration,
                3,
                2,
            );

            expected_ast.add_terminal_child(
                declaration_statement,
                Some(Token::Symbol("int32".to_owned())),
                3,
                1,
            );
            expected_ast.add_terminal_child(
                declaration_statement,
                Some(Token::Symbol("field".to_owned())),
                4,
                1,
            );
        }

        // RHS
        let declaration_statement = expected_ast.add_child(
            declaration_statements_handle,
            Rule::Declaration,
            6,
            3,
        );
        expected_ast.add_terminal_child(
            declaration_statement,
            Some(Token::Symbol("uint32".to_owned())),
            6,
            1,
        );
        expected_ast.add_terminal_child(
            declaration_statement,
            Some(Token::Symbol("field2".to_owned())),
            7,
            1,
        );

        expected_ast
    };

    check_ast_equal(&ast, &expected_ast);
}

#[test]
fn parse_struct_field_access() {
    let tokens = tokenize("a.b.c").expect("Unexpected tokenize error");
    let ast = parse(&tokens).expect("Unexpected parse error");
    let expected_ast = {
        let mut expected_ast = Ast::new();
        let root_handle = expected_ast.add_root(Rule::Expression, 0, 5);
        let primary_handle =
            expected_ast.add_child(root_handle, Rule::Primary, 0, 5);
        let struct_access_handle =
            expected_ast.add_child(primary_handle, Rule::StructAccess, 0, 5);

        // a.b
        {
            let struct_access_handle = expected_ast.add_child(
                struct_access_handle,
                Rule::StructAccess,
                0,
                3,
            );

            // a
            {
                let struct_access_handle = expected_ast.add_child(
                    struct_access_handle,
                    Rule::StructAccess,
                    0,
                    1,
                );
                expected_ast.add_terminal_child(
                    struct_access_handle,
                    Some(Token::Symbol("a".to_owned())),
                    0,
                    1,
                );
            }

            // b
            expected_ast.add_terminal_child(
                struct_access_handle,
                Some(Token::Symbol("b".to_owned())),
                2,
                1,
            );
        }

        // c
        expected_ast.add_terminal_child(
            struct_access_handle,
            Some(Token::Symbol("c".to_owned())),
            4,
            1,
        );

        expected_ast
    };

    check_ast_equal(&ast, &expected_ast);
}

#[test]
fn parse_struct_field_function_call() {
    let tokens = tokenize("a.b().c").expect("Unexpected tokenize error");
    let ast = parse(&tokens).expect("Unexpected parse error");
    let expected_ast = {
        let mut expected_ast = Ast::new();
        let root_handle = expected_ast.add_root(Rule::Expression, 0, 7);
        let primary_handle =
            expected_ast.add_child(root_handle, Rule::Primary, 0, 7);
        let struct_access_handle =
            expected_ast.add_child(primary_handle, Rule::StructAccess, 0, 7);

        // a.b()
        {
            let struct_access_handle = expected_ast.add_child(
                struct_access_handle,
                Rule::StructAccess,
                0,
                5,
            );

            // a
            {
                let struct_access_handle = expected_ast.add_child(
                    struct_access_handle,
                    Rule::StructAccess,
                    0,
                    1,
                );
                expected_ast.add_terminal_child(
                    struct_access_handle,
                    Some(Token::Symbol("a".to_owned())),
                    0,
                    1,
                );
            }

            // b()
            {
                let terminal_handle = expected_ast.add_child(
                    struct_access_handle,
                    Rule::Primary,
                    2,
                    3,
                );
                add_function_call_no_arg(
                    &mut expected_ast,
                    terminal_handle,
                    "b".to_owned(),
                    2,
                );
            }
        }

        // c
        expected_ast.add_terminal_child(
            struct_access_handle,
            Some(Token::Symbol("c".to_owned())),
            6,
            1,
        );

        expected_ast
    };

    check_ast_equal(&ast, &expected_ast);
}
