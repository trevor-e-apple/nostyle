#[cfg(test)]
mod tests {
    use std::fs::File;
    use std::path::Path;
    use std::println;
    use std::time::SystemTime;

    use crate::parse::*;

    use crate::tokenize::tokenize;

    /// helper function for adding an expression with nothing but a terminal
    /// to an ast
    fn add_terminal_expression(
        ast: &mut Ast,
        parent_handle: AstNodeHandle,
        terminal_value: Option<Token>,
    ) -> AstNodeHandle {
        let expression_handle = ast.add_child(parent_handle, Rule::Expression);
        ast.add_terminal_child(expression_handle, terminal_value);

        return expression_handle;
    }

    /// helper function for adding a child that just adds two tokens. Adds from
    /// the "equality" rule downward
    fn add_expected_add_child(
        ast: &mut Ast,
        parent_handle: AstNodeHandle,
        lhs_terminal: Token,
        rhs_terminal: Token,
    ) {
        let plus_minus_handle = ast.add_child_with_data(
            parent_handle,
            Rule::PlusMinus,
            Some(Token::Plus),
        );

        // lhs (recursive)
        ast.add_terminal_child(plus_minus_handle, Some(lhs_terminal));

        // rhs
        ast.add_terminal_child(plus_minus_handle, Some(rhs_terminal));
    }

    /// helper function for adding a child that just multiplies two tokens. Adds
    /// from the "equality" rule downward
    fn add_expected_mult_child(
        ast: &mut Ast,
        parent_handle: AstNodeHandle,
        lhs_terminal: Token,
        rhs_terminal: Token,
    ) {
        let mult_div_handle = ast.add_child_with_data(
            parent_handle,
            Rule::MultDiv,
            Some(Token::Times),
        );

        // lhs (recursive)
        ast.add_terminal_child(mult_div_handle, Some(lhs_terminal));

        // rhs
        ast.add_terminal_child(mult_div_handle, Some(rhs_terminal));
    }

    /// a helper function for adding the ast nodes for a simple assignment from
    /// one terminal to another
    fn add_assignment_statement(
        ast: &mut Ast,
        parent_handle: AstNodeHandle,
        lhs_terminal: Token,
        rhs_terminal: Token,
    ) {
        let statement_handle = ast.add_child(parent_handle, Rule::Statement);

        // LHS
        let lhs_expression = ast.add_child(statement_handle, Rule::Expression);
        ast.add_terminal_child(lhs_expression, Some(lhs_terminal));

        // RHS
        add_terminal_expression(ast, statement_handle, Some(rhs_terminal));
    }

    /// adds no statements descendents to parent
    fn add_no_statements(ast: &mut Ast, parent_handle: AstNodeHandle) {
        let brace_statements_handle =
            ast.add_child(parent_handle, Rule::BraceStatements);
        let statement_handle =
            ast.add_child(brace_statements_handle, Rule::Statement);
        add_terminal_expression(ast, statement_handle, None);
    }

    // adds tree for basic binary comparison to parent expression
    fn add_comparison_tree(
        ast: &mut Ast,
        parent_handle: AstNodeHandle,
        a: Token,
        b: Token,
    ) {
        let equality_handle = ast.add_child_with_data(
            parent_handle,
            Rule::Equality,
            Some(Token::BoolEquals),
        );

        // a
        ast.add_terminal_child(equality_handle, Some(a));

        // b
        ast.add_terminal_child(equality_handle, Some(b));
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
        let mut file =
            File::create(&file_name).expect("Unable to create dot file");
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
            let root_handle = expected_ast.add_root(Rule::Expression);
            let brace_expression_handle =
                expected_ast.add_child(root_handle, Rule::BraceExpression);
            let expression_handle = expected_ast
                .add_child(brace_expression_handle, Rule::Expression);
            expected_ast.add_terminal_child(expression_handle, None);
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
            let root_handle = expected_ast.add_root(Rule::Expression);
            let expression_handle =
                expected_ast.add_child(root_handle, Rule::Expression);
            expected_ast.add_terminal_child(expression_handle, None);
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
            let root_handle = expected_ast.add_root(Rule::Expression);
            let brace_expression_handle =
                expected_ast.add_child(root_handle, Rule::BraceExpression);

            // statements
            {
                let statements_handle = expected_ast
                    .add_child(brace_expression_handle, Rule::BraceStatements);
                let statement_handle =
                    expected_ast.add_child(statements_handle, Rule::Statement);
                let expression_handle =
                    expected_ast.add_child(statement_handle, Rule::Expression);
                expected_ast.add_terminal_child(expression_handle, None);
            }

            // end expression
            let expression_handle = expected_ast
                .add_child(brace_expression_handle, Rule::Expression);
            expected_ast.add_terminal_child(expression_handle, None);

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
                // TODO: line number check
                // TODO: check error message?
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
            let root_handle = expected_ast.add_root(Rule::Expression);
            expected_ast
                .add_terminal_child(root_handle, Some(Token::IntLiteral(0)));
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
            let root_handle = expected_ast.add_root(Rule::Expression);
            let brace_expression_handle =
                expected_ast.add_child(root_handle, Rule::BraceExpression);
            let expression_handle = expected_ast
                .add_child(brace_expression_handle, Rule::Expression);
            expected_ast.add_terminal_child(
                expression_handle,
                Some(Token::IntLiteral(0)),
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
            let root_handle = expected_ast.add_root(Rule::Expression);

            let outer_brace_expression_handle =
                expected_ast.add_child(root_handle, Rule::BraceExpression);
            let expression_handle = expected_ast
                .add_child(outer_brace_expression_handle, Rule::Expression);

            let brace_expression_handle = expected_ast
                .add_child(expression_handle, Rule::BraceExpression);
            let expression_handle = expected_ast
                .add_child(brace_expression_handle, Rule::Expression);
            expected_ast.add_terminal_child(
                expression_handle,
                Some(Token::IntLiteral(0)),
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
            let root_handle = expected_ast.add_root(Rule::Expression);
            add_expected_add_child(
                &mut expected_ast,
                root_handle,
                Token::IntLiteral(1),
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
            let root_handle = expected_ast.add_root(Rule::Expression);
            let mult_div_handle = expected_ast.add_child_with_data(
                root_handle,
                Rule::MultDiv,
                Some(Token::Times),
            );
            // 1 (recursive)
            expected_ast.add_terminal_child(
                mult_div_handle,
                Some(Token::IntLiteral(1)),
            );
            // 2
            expected_ast.add_terminal_child(
                mult_div_handle,
                Some(Token::IntLiteral(2)),
            );
            expected_ast
        };

        check_ast_equal(&ast, &expected_ast);
    }

    /// test for group on right
    #[test]
    fn expression_with_grouping_right() {
        let tokens =
            tokenize("1 + (2 + 3)").expect("Unexpected tokenize error");
        let ast = parse(&tokens).expect("Unexpected parse error");

        let expected_ast = {
            let mut expected_ast = Ast::new();
            let root_handle = expected_ast.add_root(Rule::Expression);
            let plus_minus_handle = expected_ast.add_child_with_data(
                root_handle,
                Rule::PlusMinus,
                Some(Token::Plus),
            );

            // LHS: 1
            expected_ast.add_terminal_child(
                plus_minus_handle,
                Some(Token::IntLiteral(1)),
            );

            // RHS: (2 + 3)
            {
                // 2 + 3
                let expression_handle =
                    expected_ast.add_child(plus_minus_handle, Rule::Expression);
                add_expected_add_child(
                    &mut expected_ast,
                    expression_handle,
                    Token::IntLiteral(2),
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
        let tokens =
            tokenize("(1 + 2) * 3").expect("Unexpected tokenize error");
        let ast = parse(&tokens).expect("Unexpected parse error");

        let expected_ast = {
            let mut expected_ast = Ast::new();
            let root_handle = expected_ast.add_root(Rule::Expression);
            let mult_div_handle = expected_ast.add_child_with_data(
                root_handle,
                Rule::MultDiv,
                Some(Token::Times),
            );

            // LHS: (1 + 2)
            {
                let expression_handle =
                    expected_ast.add_child(mult_div_handle, Rule::Expression);
                add_expected_add_child(
                    &mut expected_ast,
                    expression_handle,
                    Token::IntLiteral(1),
                    Token::IntLiteral(2),
                );
            }

            // RHS: 3
            expected_ast.add_terminal_child(
                mult_div_handle,
                Some(Token::IntLiteral(3)),
            );

            expected_ast
        };

        check_ast_equal(&ast, &expected_ast);
    }

    #[test]
    fn expression_with_brace_grouping() {
        let tokens =
            tokenize("1 + {2 + 3}").expect("Unexpected tokenize error");
        let ast = parse(&tokens).expect("Unexpected parse error");

        let expected_ast = {
            let mut expected_ast = Ast::new();
            let root_handle = expected_ast.add_root(Rule::Expression);
            let plus_minus_handle = expected_ast.add_child_with_data(
                root_handle,
                Rule::PlusMinus,
                Some(Token::Plus),
            );

            // LHS: 1
            expected_ast.add_terminal_child(
                plus_minus_handle,
                Some(Token::IntLiteral(1)),
            );

            // RHS: {2 + 3}
            {
                // 2 + 3
                let brace_expression_handle = expected_ast
                    .add_child(plus_minus_handle, Rule::BraceExpression);
                let expression_handle = expected_ast
                    .add_child(brace_expression_handle, Rule::Expression);
                add_expected_add_child(
                    &mut expected_ast,
                    expression_handle,
                    Token::IntLiteral(2),
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
            let root_handle = expected_ast.add_root(Rule::Expression);
            add_expected_add_child(
                &mut expected_ast,
                root_handle,
                Token::Symbol("a".to_owned()),
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
            let root_handle = expected_ast.add_root(Rule::Expression);
            let brace_expression_handle =
                expected_ast.add_child(root_handle, Rule::BraceExpression);

            // statements
            {
                let statements_handle = expected_ast
                    .add_child(brace_expression_handle, Rule::BraceStatements);
                add_assignment_statement(
                    &mut expected_ast,
                    statements_handle,
                    Token::Symbol("a".to_owned()),
                    Token::Symbol("b".to_owned()),
                );
            }

            // no expression at end of brace expression
            add_terminal_expression(
                &mut expected_ast,
                brace_expression_handle,
                None,
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
            let root_handle = expected_ast.add_root(Rule::Expression);
            let brace_expression_handle =
                expected_ast.add_child(root_handle, Rule::BraceExpression);

            // no expression at end of brace expression
            add_terminal_expression(
                &mut expected_ast,
                brace_expression_handle,
                Some(Token::Symbol("a".to_owned())),
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
            let root_handle = expected_ast.add_root(Rule::Expression);
            let brace_expression_handle =
                expected_ast.add_child(root_handle, Rule::BraceExpression);

            let expression_handle = expected_ast
                .add_child(brace_expression_handle, Rule::Expression);

            add_expected_add_child(
                &mut expected_ast,
                expression_handle,
                Token::Symbol("a".to_owned()),
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
            let root_handle = expected_ast.add_root(Rule::Expression);
            let brace_expression_handle =
                expected_ast.add_child(root_handle, Rule::BraceExpression);

            // statements
            {
                let statements_handle = expected_ast
                    .add_child(brace_expression_handle, Rule::BraceStatements);
                let statement_handle =
                    expected_ast.add_child(statements_handle, Rule::Statement);

                let expression_handle =
                    expected_ast.add_child(statement_handle, Rule::Expression);
                expected_ast.add_terminal_child(
                    expression_handle,
                    Some(Token::Symbol("a".to_owned())),
                );
            }

            // no expression at end of brace expression
            add_terminal_expression(
                &mut expected_ast,
                brace_expression_handle,
                None,
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
            let root_handle = expected_ast.add_root(Rule::Expression);
            let brace_expression_handle =
                expected_ast.add_child(root_handle, Rule::BraceExpression);

            // statements
            {
                let statements_handle = expected_ast
                    .add_child(brace_expression_handle, Rule::BraceStatements);
                let statement_handle =
                    expected_ast.add_child(statements_handle, Rule::Statement);

                // a + b
                let expression_handle =
                    expected_ast.add_child(statement_handle, Rule::Expression);
                add_expected_add_child(
                    &mut expected_ast,
                    expression_handle,
                    Token::Symbol("a".to_owned()),
                    Token::Symbol("b".to_owned()),
                );
            }

            // no expression at end of braces
            add_terminal_expression(
                &mut expected_ast,
                brace_expression_handle,
                None,
            );

            expected_ast
        };

        check_ast_equal(&ast, &expected_ast);
    }

    #[test]
    fn assign_expression() {
        let tokens =
            tokenize("{ a = b + c; }").expect("Unexpected tokenize error");
        let ast = parse(&tokens).expect("Unexpected parse error");

        let expected_ast = {
            let mut expected_ast = Ast::new();
            let root_handle = expected_ast.add_root(Rule::Expression);
            let brace_expression_handle =
                expected_ast.add_child(root_handle, Rule::BraceExpression);

            // statements
            {
                let statements_handle = expected_ast
                    .add_child(brace_expression_handle, Rule::BraceStatements);
                let statement_handle =
                    expected_ast.add_child(statements_handle, Rule::Statement);

                // a = b + c
                {
                    // lhs: a
                    add_terminal_expression(
                        &mut expected_ast,
                        statement_handle,
                        Some(Token::Symbol("a".to_owned())),
                    );

                    // rhs: b + c
                    {
                        let expression_handle = expected_ast
                            .add_child(statement_handle, Rule::Expression);
                        add_expected_add_child(
                            &mut expected_ast,
                            expression_handle,
                            Token::Symbol("b".to_owned()),
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

            let root_handle = expected_ast.add_root(Rule::Expression);
            let brace_expression_handle =
                expected_ast.add_child(root_handle, Rule::BraceExpression);

            // statements
            {
                let brace_statements_handle = expected_ast
                    .add_child(brace_expression_handle, Rule::BraceStatements);
                let statement_handle = expected_ast
                    .add_child(brace_statements_handle, Rule::Statement);

                // lhs: a
                add_terminal_expression(
                    &mut expected_ast,
                    statement_handle,
                    Some(Token::Symbol("a".to_owned())),
                );

                // rhs: {b + c}
                {
                    let expression_handle = expected_ast
                        .add_child(statement_handle, Rule::Expression);
                    let brace_expression_handle = expected_ast
                        .add_child(expression_handle, Rule::BraceExpression);

                    // expression
                    {
                        let rhs_expression_handle = expected_ast.add_child(
                            brace_expression_handle,
                            Rule::Expression,
                        );
                        add_expected_add_child(
                            &mut expected_ast,
                            rhs_expression_handle,
                            Token::Symbol("b".to_owned()),
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

            let root_handle = expected_ast.add_root(Rule::Expression);
            let brace_expression_handle =
                expected_ast.add_child(root_handle, Rule::BraceExpression);

            // statements
            {
                let brace_statements_handle = expected_ast
                    .add_child(brace_expression_handle, Rule::BraceStatements);
                let statement_handle = expected_ast
                    .add_child(brace_statements_handle, Rule::Statement);

                // lhs: a
                add_terminal_expression(
                    &mut expected_ast,
                    statement_handle,
                    Some(Token::Symbol("a".to_owned())),
                );

                // rhs: {b = c + d; a + b}
                {
                    let expression_handle = expected_ast
                        .add_child(statement_handle, Rule::Expression);
                    let brace_expression_handle = expected_ast
                        .add_child(expression_handle, Rule::BraceExpression);

                    // statements
                    {
                        let brace_statements_handle = expected_ast.add_child(
                            brace_expression_handle,
                            Rule::BraceStatements,
                        );
                        let statement_handle = expected_ast.add_child(
                            brace_statements_handle,
                            Rule::Statement,
                        );

                        // b = c + d;
                        {
                            // lhs
                            add_terminal_expression(
                                &mut expected_ast,
                                statement_handle,
                                Some(Token::Symbol("b".to_owned())),
                            );

                            // rhs
                            {
                                let expression_handle = expected_ast.add_child(
                                    statement_handle,
                                    Rule::Expression,
                                );
                                add_expected_add_child(
                                    &mut expected_ast,
                                    expression_handle,
                                    Token::Symbol("c".to_owned()),
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
                        );
                        add_expected_add_child(
                            &mut expected_ast,
                            expression_handle,
                            Token::Symbol("a".to_owned()),
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
            let root_handle = expected_ast.add_root(Rule::Expression);
            let plus_minus_handle = expected_ast.add_child_with_data(
                root_handle,
                Rule::PlusMinus,
                Some(Token::Minus),
            );

            // a + b
            {
                let a_plus_b_handle = expected_ast.add_child_with_data(
                    plus_minus_handle,
                    Rule::PlusMinus,
                    Some(Token::Plus),
                );

                // a
                expected_ast.add_terminal_child(
                    a_plus_b_handle,
                    Some(Token::Symbol("a".to_owned())),
                );

                // b
                expected_ast.add_terminal_child(
                    a_plus_b_handle,
                    Some(Token::Symbol("b".to_owned())),
                );
            }

            // - c
            expected_ast.add_terminal_child(
                plus_minus_handle,
                Some(Token::Symbol("c".to_owned())),
            );

            expected_ast
        };

        check_ast_equal(&ast, &expected_ast);
    }

    #[test]
    fn add_mult_precedence() {
        let tokens = tokenize("a + b * c").expect("Unexpected tokenize error");
        let ast = parse(&tokens).expect("Unexpected parse errror");

        let expected_ast = {
            let mut expected_ast = Ast::new();
            let root_handle = expected_ast.add_root(Rule::Expression);
            let plus_minus_handle = expected_ast.add_child_with_data(
                root_handle,
                Rule::PlusMinus,
                Some(Token::Plus),
            );

            // a
            expected_ast.add_terminal_child(
                plus_minus_handle,
                Some(Token::Symbol("a".to_owned())),
            );

            // b * c
            {
                let mult_div_handle = expected_ast.add_child_with_data(
                    plus_minus_handle,
                    Rule::MultDiv,
                    Some(Token::Times),
                );

                // b
                expected_ast.add_terminal_child(
                    mult_div_handle,
                    Some(Token::Symbol("b".to_owned())),
                );

                // c
                expected_ast.add_terminal_child(
                    mult_div_handle,
                    Some(Token::Symbol("c".to_owned())),
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
        let ast = parse(&tokens).expect("Unexpected parse errror");
        let expected_ast = {
            let mut expected_ast = Ast::new();

            let root_handle = expected_ast.add_root(Rule::Expression);

            let mult_div_handle = expected_ast.add_child_with_data(
                root_handle,
                Rule::MultDiv,
                Some(Token::Times),
            );

            // LHS: a
            expected_ast.add_terminal_child(
                mult_div_handle,
                Some(Token::Symbol("a".to_owned())),
            );

            // RHS: (b - (c + d))
            {
                let expression_handle =
                    expected_ast.add_child(mult_div_handle, Rule::Expression);
                let plus_minus_handle = expected_ast.add_child_with_data(
                    expression_handle,
                    Rule::PlusMinus,
                    Some(Token::Minus),
                );

                // LHS: b
                expected_ast.add_terminal_child(
                    plus_minus_handle,
                    Some(Token::Symbol("b".to_owned())),
                );

                // RHS: (c + d)
                {
                    let expression_handle = expected_ast
                        .add_child(plus_minus_handle, Rule::Expression);
                    let plus_minus_handle = expected_ast.add_child_with_data(
                        expression_handle,
                        Rule::PlusMinus,
                        Some(Token::Plus),
                    );

                    // LHS: c
                    expected_ast.add_terminal_child(
                        plus_minus_handle,
                        Some(Token::Symbol("c".to_owned())),
                    );

                    // RHS: d
                    expected_ast.add_terminal_child(
                        plus_minus_handle,
                        Some(Token::Symbol("d".to_owned())),
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
        let ast = parse(&tokens).expect("Unexpected parse errror");

        let expected_ast = {
            let mut expected_ast = Ast::new();
            let root_handle = expected_ast.add_root(Rule::Expression);
            let brace_expression_handle =
                expected_ast.add_child(root_handle, Rule::BraceExpression);

            // statements
            {
                let statements_handle = expected_ast
                    .add_child(brace_expression_handle, Rule::BraceStatements);

                // a = b;
                {
                    let statements_handle = expected_ast
                        .add_child(statements_handle, Rule::BraceStatements);

                    // a = b;
                    add_assignment_statement(
                        &mut expected_ast,
                        statements_handle,
                        Token::Symbol("a".to_owned()),
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
                    let statement_handle = expected_ast
                        .add_child(statements_handle, Rule::Statement);

                    // LHS: c
                    add_terminal_expression(
                        &mut expected_ast,
                        statement_handle,
                        Some(Token::Symbol("c".to_owned())),
                    );

                    // RHS
                    /*
                    {
                        d = 2 * a;
                        d
                    };
                    */
                    {
                        let expression_handle = expected_ast
                            .add_child(statement_handle, Rule::Expression);
                        let brace_expression_handle = expected_ast.add_child(
                            expression_handle,
                            Rule::BraceExpression,
                        );

                        {
                            let statements_handle = expected_ast.add_child(
                                brace_expression_handle,
                                Rule::BraceStatements,
                            );

                            // d = 2 * a;
                            {
                                let statement_handle = expected_ast.add_child(
                                    statements_handle,
                                    Rule::Statement,
                                );

                                // LHS: d
                                add_terminal_expression(
                                    &mut expected_ast,
                                    statement_handle,
                                    Some(Token::Symbol("d".to_owned())),
                                );

                                // RHS: 2 * a
                                let expression_handle = expected_ast.add_child(
                                    statement_handle,
                                    Rule::Expression,
                                );
                                add_expected_mult_child(
                                    &mut expected_ast,
                                    expression_handle,
                                    Token::IntLiteral(2),
                                    Token::Symbol("a".to_owned()),
                                );
                            }
                        }

                        // expression at end
                        add_terminal_expression(
                            &mut expected_ast,
                            brace_expression_handle,
                            Some(Token::Symbol("d".to_owned())),
                        );
                    }
                }
            }

            // a + c
            {
                let end_expression_handle = expected_ast
                    .add_child(brace_expression_handle, Rule::Expression);
                add_expected_add_child(
                    &mut expected_ast,
                    end_expression_handle,
                    Token::Symbol("a".to_owned()),
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
        let ast = parse(&tokens).expect("Unexpected parse errror");
        let expected_ast = {
            let mut expected_ast = Ast::new();
            let root_handle = expected_ast.add_root(Rule::Expression);
            let brace_expression_handle =
                expected_ast.add_child(root_handle, Rule::BraceExpression);

            // statements
            {
                let statements_handle = expected_ast
                    .add_child(brace_expression_handle, Rule::BraceStatements);

                /*
                c = {
                    d = 2 * a;
                    d
                };
                */
                {
                    let statements_handle = expected_ast
                        .add_child(statements_handle, Rule::BraceStatements);

                    let statement_handle = expected_ast
                        .add_child(statements_handle, Rule::Statement);

                    // LHS: c
                    add_terminal_expression(
                        &mut expected_ast,
                        statement_handle,
                        Some(Token::Symbol("c".to_owned())),
                    );

                    // RHS
                    /*
                    {
                        d = 2 * a;
                        d
                    };
                    */
                    {
                        let expression_handle = expected_ast
                            .add_child(statement_handle, Rule::Expression);
                        let brace_expression_handle = expected_ast.add_child(
                            expression_handle,
                            Rule::BraceExpression,
                        );

                        {
                            let statements_handle = expected_ast.add_child(
                                brace_expression_handle,
                                Rule::BraceStatements,
                            );

                            // d = 2 * a;
                            {
                                let statement_handle = expected_ast.add_child(
                                    statements_handle,
                                    Rule::Statement,
                                );

                                // LHS: d
                                add_terminal_expression(
                                    &mut expected_ast,
                                    statement_handle,
                                    Some(Token::Symbol("d".to_owned())),
                                );

                                // RHS: 2 * a
                                let expression_handle = expected_ast.add_child(
                                    statement_handle,
                                    Rule::Expression,
                                );
                                add_expected_mult_child(
                                    &mut expected_ast,
                                    expression_handle,
                                    Token::IntLiteral(2),
                                    Token::Symbol("a".to_owned()),
                                );
                            }
                        }

                        // expression at end
                        add_terminal_expression(
                            &mut expected_ast,
                            brace_expression_handle,
                            Some(Token::Symbol("d".to_owned())),
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
                        Token::Symbol("b".to_owned()),
                    );
                }
            }

            // a + c
            {
                let end_expression_handle = expected_ast
                    .add_child(brace_expression_handle, Rule::Expression);
                add_expected_add_child(
                    &mut expected_ast,
                    end_expression_handle,
                    Token::Symbol("a".to_owned()),
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
        let ast = parse(&tokens).expect("Unexpected parse errror");
        let expected_ast = {
            let mut expected_ast = Ast::new();
            let root_handle = expected_ast.add_root(Rule::Expression);
            let if_else_handle =
                expected_ast.add_child(root_handle, Rule::IfElse);
            // condition expression
            {
                let condition_expression_handle =
                    expected_ast.add_child(if_else_handle, Rule::Expression);
                let equality_handle = expected_ast.add_child_with_data(
                    condition_expression_handle,
                    Rule::Equality,
                    Some(Token::BoolEquals),
                );
                // (a + b)
                {
                    let expression_handle = expected_ast
                        .add_child(equality_handle, Rule::Expression);
                    add_expected_add_child(
                        &mut expected_ast,
                        expression_handle,
                        Token::Symbol("a".to_owned()),
                        Token::Symbol("b".to_owned()),
                    );
                }
                // c
                expected_ast.add_terminal_child(
                    equality_handle,
                    Some(Token::Symbol("c".to_owned())),
                );
            }
            // executed brace_expression
            {
                let brace_expression = expected_ast
                    .add_child(if_else_handle, Rule::BraceExpression);
                // brace statements
                {
                    let brace_statements_handle = expected_ast
                        .add_child(brace_expression, Rule::BraceStatements);
                    add_assignment_statement(
                        &mut expected_ast,
                        brace_statements_handle,
                        Token::Symbol("d".to_owned()),
                        Token::Symbol("c".to_owned()),
                    );
                }

                // no ending expression
                add_terminal_expression(
                    &mut expected_ast,
                    brace_expression,
                    None,
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
        let ast = parse(&tokens).expect("Unexpected parse errror");
        let expected_ast = {
            let mut expected_ast = Ast::new();
            let root_handle = expected_ast.add_root(Rule::Expression);
            let if_else_handle =
                expected_ast.add_child(root_handle, Rule::IfElse);

            // condition expression
            {
                let condition_expression_handle =
                    expected_ast.add_child(if_else_handle, Rule::Expression);
                let equality_handle = expected_ast.add_child_with_data(
                    condition_expression_handle,
                    Rule::Equality,
                    Some(Token::BoolEquals),
                );
                // (a + b)
                {
                    let expression_handle = expected_ast
                        .add_child(equality_handle, Rule::Expression);
                    add_expected_add_child(
                        &mut expected_ast,
                        expression_handle,
                        Token::Symbol("a".to_owned()),
                        Token::Symbol("b".to_owned()),
                    );
                }
                // c
                expected_ast.add_terminal_child(
                    equality_handle,
                    Some(Token::Symbol("c".to_owned())),
                );
            }
            // executed brace_expression
            {
                let brace_expression = expected_ast
                    .add_child(if_else_handle, Rule::BraceExpression);
                // brace statements
                {
                    let brace_statements_handle = expected_ast
                        .add_child(brace_expression, Rule::BraceStatements);
                    add_assignment_statement(
                        &mut expected_ast,
                        brace_statements_handle,
                        Token::Symbol("d".to_owned()),
                        Token::Symbol("c".to_owned()),
                    );
                }

                // no ending expression
                add_terminal_expression(
                    &mut expected_ast,
                    brace_expression,
                    None,
                );
            }

            // else
            {
                let expression_handle =
                    expected_ast.add_child(if_else_handle, Rule::Expression);
                let brace_expression_handle = expected_ast
                    .add_child(expression_handle, Rule::BraceExpression);

                // statements
                let brace_statements_handle = expected_ast
                    .add_child(brace_expression_handle, Rule::BraceStatements);
                add_assignment_statement(
                    &mut expected_ast,
                    brace_statements_handle,
                    Token::Symbol("d".to_owned()),
                    Token::Symbol("e".to_owned()),
                );
                // expression
                add_terminal_expression(
                    &mut expected_ast,
                    brace_expression_handle,
                    None,
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
        let ast = parse(&tokens).expect("Unexpected parse errror");

        let expected_ast = {
            let mut expected_ast = Ast::new();
            let root_handle = expected_ast.add_root(Rule::Expression);
            let if_else_handle =
                expected_ast.add_child(root_handle, Rule::IfElse);

            // a == b
            {
                let condition_expression_handle =
                    expected_ast.add_child(if_else_handle, Rule::Expression);

                add_comparison_tree(
                    &mut expected_ast,
                    condition_expression_handle,
                    Token::Symbol("a".to_owned()),
                    Token::Symbol("b".to_owned()),
                );
            }
            // { d = b; }
            {
                let brace_expression = expected_ast
                    .add_child(if_else_handle, Rule::BraceExpression);
                // d = b;
                {
                    let brace_statements_handle = expected_ast
                        .add_child(brace_expression, Rule::BraceStatements);
                    add_assignment_statement(
                        &mut expected_ast,
                        brace_statements_handle,
                        Token::Symbol("d".to_owned()),
                        Token::Symbol("b".to_owned()),
                    );
                }

                // no ending expression
                add_terminal_expression(
                    &mut expected_ast,
                    brace_expression,
                    None,
                );
            }

            // else
            {
                let expression_handle =
                    expected_ast.add_child(if_else_handle, Rule::Expression);

                let if_else_handle =
                    expected_ast.add_child(expression_handle, Rule::IfElse);

                // a == c
                {
                    let condition_expression_handle = expected_ast
                        .add_child(if_else_handle, Rule::Expression);
                    add_comparison_tree(
                        &mut expected_ast,
                        condition_expression_handle,
                        Token::Symbol("a".to_owned()),
                        Token::Symbol("c".to_owned()),
                    );
                }

                // { d = c; }
                {
                    let brace_expression = expected_ast
                        .add_child(if_else_handle, Rule::BraceExpression);
                    // d = c;
                    {
                        let brace_statements_handle = expected_ast
                            .add_child(brace_expression, Rule::BraceStatements);
                        add_assignment_statement(
                            &mut expected_ast,
                            brace_statements_handle,
                            Token::Symbol("d".to_owned()),
                            Token::Symbol("c".to_owned()),
                        );
                    }

                    // no ending expression
                    add_terminal_expression(
                        &mut expected_ast,
                        brace_expression,
                        None,
                    );
                }

                // else
                {
                    let expression_handle = expected_ast
                        .add_child(if_else_handle, Rule::Expression);
                    let brace_expression_handle = expected_ast
                        .add_child(expression_handle, Rule::BraceExpression);

                    // statements
                    let brace_statements_handle = expected_ast.add_child(
                        brace_expression_handle,
                        Rule::BraceStatements,
                    );
                    add_assignment_statement(
                        &mut expected_ast,
                        brace_statements_handle,
                        Token::Symbol("d".to_owned()),
                        Token::Symbol("e".to_owned()),
                    );
                    // expression
                    add_terminal_expression(
                        &mut expected_ast,
                        brace_expression_handle,
                        None,
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
        let ast = parse(&tokens).expect("Unexpected parse errror");

        let expected_ast = {
            let mut expected_ast = Ast::new();
            let root_handle = expected_ast.add_root(Rule::Expression);
            let brace_expression_handle =
                expected_ast.add_child(root_handle, Rule::BraceExpression);
            let statements_handle = expected_ast
                .add_child(brace_expression_handle, Rule::BraceStatements);

            let statement_handle =
                expected_ast.add_child(statements_handle, Rule::Statement);

            // Statement LHS
            add_terminal_expression(
                &mut expected_ast,
                statement_handle,
                Some(Token::Symbol("d".to_owned())),
            );

            // Statement RHS
            {
                let expression_handle =
                    expected_ast.add_child(statement_handle, Rule::Expression);
                let if_else_handle =
                    expected_ast.add_child(expression_handle, Rule::IfElse);

                // a == b
                {
                    let condition_expression_handle = expected_ast
                        .add_child(if_else_handle, Rule::Expression);
                    add_comparison_tree(
                        &mut expected_ast,
                        condition_expression_handle,
                        Token::Symbol("a".to_owned()),
                        Token::Symbol("b".to_owned()),
                    );
                }
                // { b }
                {
                    let brace_expression = expected_ast
                        .add_child(if_else_handle, Rule::BraceExpression);

                    // b
                    add_terminal_expression(
                        &mut expected_ast,
                        brace_expression,
                        Some(Token::Symbol("b".to_owned())),
                    );
                }

                // else
                {
                    let expression_handle = expected_ast
                        .add_child(if_else_handle, Rule::Expression);

                    let if_else_handle =
                        expected_ast.add_child(expression_handle, Rule::IfElse);

                    // a == c
                    {
                        let condition_expression_handle = expected_ast
                            .add_child(if_else_handle, Rule::Expression);
                        add_comparison_tree(
                            &mut expected_ast,
                            condition_expression_handle,
                            Token::Symbol("a".to_owned()),
                            Token::Symbol("c".to_owned()),
                        );
                    }

                    // { c }
                    {
                        let brace_expression = expected_ast
                            .add_child(if_else_handle, Rule::BraceExpression);

                        // c
                        add_terminal_expression(
                            &mut expected_ast,
                            brace_expression,
                            Some(Token::Symbol("c".to_owned())),
                        );
                    }

                    // else
                    {
                        let expression_handle = expected_ast
                            .add_child(if_else_handle, Rule::Expression);
                        let brace_expression = expected_ast.add_child(
                            expression_handle,
                            Rule::BraceExpression,
                        );

                        // e
                        add_terminal_expression(
                            &mut expected_ast,
                            brace_expression,
                            Some(Token::Symbol("e".to_owned())),
                        );
                    }
                }
            }

            // no terminal expression
            add_terminal_expression(
                &mut expected_ast,
                brace_expression_handle,
                None,
            );

            expected_ast
        };

        check_ast_equal(&ast, &expected_ast);
    }

    fn add_for_loop_declaration(
        ast: &mut Ast,
        parent_handle: AstNodeHandle,
    ) -> AstNodeHandle {
        let for_handle = ast.add_child(parent_handle, Rule::ForLoop);

        // init statement
        add_assignment_statement(
            ast,
            for_handle,
            Token::Symbol("a".to_owned()),
            Token::IntLiteral(0),
        );

        // condition statement
        {
            let condition_statement =
                ast.add_child(for_handle, Rule::Statement);
            let condition_expression =
                ast.add_child(condition_statement, Rule::Expression);
            let comparison_handle = ast.add_child_with_data(
                condition_expression,
                Rule::Comparison,
                Some(Token::LessThan),
            );

            ast.add_terminal_child(
                comparison_handle,
                Some(Token::Symbol("a".to_owned())),
            );

            // terminal side
            ast.add_terminal_child(
                comparison_handle,
                Some(Token::IntLiteral(10)),
            );
        }
        // increment
        {
            let statement_handle = ast.add_child(for_handle, Rule::Statement);

            // lhs
            add_terminal_expression(
                ast,
                statement_handle,
                Some(Token::Symbol("a".to_owned())),
            );

            // rhs
            {
                let expression_handle =
                    ast.add_child(statement_handle, Rule::Expression);
                add_expected_add_child(
                    ast,
                    expression_handle,
                    Token::Symbol("a".to_owned()),
                    Token::IntLiteral(1),
                )
            }
        }

        for_handle
    }

    fn add_basic_for_loop(
        ast: &mut Ast,
        parent_handle: AstNodeHandle,
    ) -> AstNodeHandle {
        let for_handle = add_for_loop_declaration(ast, parent_handle);

        // brace_expression
        {
            let brace_expression =
                ast.add_child(for_handle, Rule::BraceExpression);
            // brace statements
            {
                let brace_statements =
                    ast.add_child(brace_expression, Rule::BraceStatements);
                let statement_handle =
                    ast.add_child(brace_statements, Rule::Statement);

                // statement lhs: assignment
                add_terminal_expression(
                    ast,
                    statement_handle,
                    Some(Token::Symbol("b".to_owned())),
                );

                // statement rhs: expression
                {
                    let rhs_expression =
                        ast.add_child(statement_handle, Rule::Expression);
                    add_expected_mult_child(
                        ast,
                        rhs_expression,
                        Token::IntLiteral(2),
                        Token::Symbol("b".to_owned()),
                    );
                }
            }

            // expression
            add_terminal_expression(ast, brace_expression, None);
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
        let ast = parse(&tokens).expect("Unexpected parse errror");

        let expected_ast = {
            let mut expected_ast = Ast::new();
            let root_handle = expected_ast.add_root(Rule::Expression);
            add_basic_for_loop(&mut expected_ast, root_handle);

            expected_ast
        };
        check_ast_equal(&ast, &expected_ast);
    }

    #[test]
    fn nested_assignment_in_brace_expressions() {
        let tokens =
            tokenize("{{a = 0;}; a}").expect("Unexpected tokenize error");
        let ast = parse(&tokens).expect("Unexpected parse errror");

        let expected_ast = {
            let mut expected_ast = Ast::new();
            let root_handle = expected_ast.add_root(Rule::Expression);

            let brace_expression =
                expected_ast.add_child(root_handle, Rule::BraceExpression);

            // {a = 0;};
            {
                let brace_statements = expected_ast
                    .add_child(brace_expression, Rule::BraceStatements);
                let statement =
                    expected_ast.add_child(brace_statements, Rule::Statement);
                let expression =
                    expected_ast.add_child(statement, Rule::Expression);
                let brace_expression =
                    expected_ast.add_child(expression, Rule::BraceExpression);

                // a = 0;
                {
                    let brace_statements = expected_ast
                        .add_child(brace_expression, Rule::BraceStatements);

                    add_assignment_statement(
                        &mut expected_ast,
                        brace_statements,
                        Token::Symbol("a".to_owned()),
                        Token::IntLiteral(0),
                    );
                }
                // ending expression
                add_terminal_expression(
                    &mut expected_ast,
                    brace_expression,
                    None,
                );
            }

            // ending expression
            add_terminal_expression(
                &mut expected_ast,
                brace_expression,
                Some(Token::Symbol("a".to_owned())),
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
        let ast = parse(&tokens).expect("Unexpected parse errror");

        let expected_ast = {
            let mut expected_ast = Ast::new();
            let root_handle = expected_ast.add_root(Rule::Expression);
            let for_handle = expected_ast.add_child(root_handle, Rule::ForLoop);

            // init statement
            add_assignment_statement(
                &mut expected_ast,
                for_handle,
                Token::Symbol("a".to_owned()),
                Token::IntLiteral(0),
            );

            // condition statement
            {
                let condition_statement =
                    expected_ast.add_child(for_handle, Rule::Statement);
                let condition_expression = expected_ast
                    .add_child(condition_statement, Rule::Expression);
                let comparison_handle = expected_ast.add_child_with_data(
                    condition_expression,
                    Rule::Comparison,
                    Some(Token::LessThan),
                );

                expected_ast.add_terminal_child(
                    comparison_handle,
                    Some(Token::Symbol("a".to_owned())),
                );

                // terminal side
                expected_ast.add_terminal_child(
                    comparison_handle,
                    Some(Token::IntLiteral(10)),
                );
            }

            // increment
            {
                let statement_handle =
                    expected_ast.add_child(for_handle, Rule::Statement);

                // lhs
                add_terminal_expression(
                    &mut expected_ast,
                    statement_handle,
                    Some(Token::Symbol("a".to_owned())),
                );

                // rhs
                {
                    let expression_handle = expected_ast
                        .add_child(statement_handle, Rule::Expression);
                    let brace_expression_handle = expected_ast
                        .add_child(expression_handle, Rule::BraceExpression);
                    let end_expression_handle = expected_ast
                        .add_child(brace_expression_handle, Rule::Expression);
                    let paren_group_handle = expected_ast
                        .add_child(end_expression_handle, Rule::Expression);
                    add_expected_add_child(
                        &mut expected_ast,
                        paren_group_handle,
                        Token::Symbol("a".to_owned()),
                        Token::IntLiteral(1),
                    )
                }
            }

            // brace_expression
            {
                let brace_expression =
                    expected_ast.add_child(for_handle, Rule::BraceExpression);
                // brace statements
                {
                    let brace_statements = expected_ast
                        .add_child(brace_expression, Rule::BraceStatements);
                    let statement_handle = expected_ast
                        .add_child(brace_statements, Rule::Statement);

                    // statement lhs: assignment
                    add_terminal_expression(
                        &mut expected_ast,
                        statement_handle,
                        Some(Token::Symbol("b".to_owned())),
                    );

                    // statement rhs: expression
                    {
                        let rhs_expression = expected_ast
                            .add_child(statement_handle, Rule::Expression);
                        add_expected_mult_child(
                            &mut expected_ast,
                            rhs_expression,
                            Token::IntLiteral(2),
                            Token::Symbol("b".to_owned()),
                        );
                    }
                }

                // expression
                add_terminal_expression(
                    &mut expected_ast,
                    brace_expression,
                    None,
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
        let ast = parse(&tokens).expect("Unexpected parse errror");

        let expected_ast = {
            let mut expected_ast = Ast::new();
            let root_handle = expected_ast.add_root(Rule::Expression);
            let brace_expression_handle =
                expected_ast.add_child(root_handle, Rule::BraceExpression);

            // statements
            {
                let statements_handle = expected_ast
                    .add_child(brace_expression_handle, Rule::BraceStatements);

                // recursive statements
                {
                    let statements_handle = expected_ast
                        .add_child(statements_handle, Rule::BraceStatements);

                    // recursive statements
                    {
                        let statements_handle = expected_ast.add_child(
                            statements_handle,
                            Rule::BraceStatements,
                        );

                        // a = b;
                        add_assignment_statement(
                            &mut expected_ast,
                            statements_handle,
                            Token::Symbol("a".to_owned()),
                            Token::Symbol("b".to_owned()),
                        );
                    }

                    // c = d;
                    add_assignment_statement(
                        &mut expected_ast,
                        statements_handle,
                        Token::Symbol("c".to_owned()),
                        Token::Symbol("d".to_owned()),
                    );
                }

                // e = f;
                add_assignment_statement(
                    &mut expected_ast,
                    statements_handle,
                    Token::Symbol("e".to_owned()),
                    Token::Symbol("f".to_owned()),
                );
            }

            // no expression at end of braces
            add_terminal_expression(
                &mut expected_ast,
                brace_expression_handle,
                None,
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
        let ast = parse(&tokens).expect("Unexpected parse errror");

        let expected_ast = {
            let mut expected_ast = Ast::new();
            let root_handle = expected_ast.add_root(Rule::Expression);
            let brace_expression_handle =
                expected_ast.add_child(root_handle, Rule::BraceExpression);

            // statements
            {
                let statements_handle = expected_ast
                    .add_child(brace_expression_handle, Rule::BraceStatements);

                // recursive statements
                {
                    let statements_handle = expected_ast
                        .add_child(statements_handle, Rule::BraceStatements);

                    // recursive statements
                    {
                        let statements_handle = expected_ast.add_child(
                            statements_handle,
                            Rule::BraceStatements,
                        );

                        // a = b;
                        add_assignment_statement(
                            &mut expected_ast,
                            statements_handle,
                            Token::Symbol("a".to_owned()),
                            Token::Symbol("b".to_owned()),
                        );
                    }

                    // c = d;
                    add_assignment_statement(
                        &mut expected_ast,
                        statements_handle,
                        Token::Symbol("c".to_owned()),
                        Token::Symbol("d".to_owned()),
                    );
                }

                // e = f;
                add_assignment_statement(
                    &mut expected_ast,
                    statements_handle,
                    Token::Symbol("e".to_owned()),
                    Token::Symbol("f".to_owned()),
                );
            }

            // expression at the end
            add_terminal_expression(
                &mut expected_ast,
                brace_expression_handle,
                Some(Token::Symbol("e".to_owned())),
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
        let ast = parse(&tokens).expect("Unexpected parse errror");
        let expected_ast = {
            let mut expected_ast = Ast::new();
            let root_handle = expected_ast.add_root(Rule::Expression);
            let brace_expression_handle =
                expected_ast.add_child(root_handle, Rule::BraceExpression);

            // statements
            {
                let statements_handle = expected_ast
                    .add_child(brace_expression_handle, Rule::BraceStatements);

                // recursive statements
                {
                    let statements_handle = expected_ast
                        .add_child(statements_handle, Rule::BraceStatements);

                    // recursive statements
                    {
                        let statements_handle = expected_ast.add_child(
                            statements_handle,
                            Rule::BraceStatements,
                        );

                        // a = b;
                        add_assignment_statement(
                            &mut expected_ast,
                            statements_handle,
                            Token::Symbol("a".to_owned()),
                            Token::Symbol("b".to_owned()),
                        );
                    }

                    // c = d;
                    add_assignment_statement(
                        &mut expected_ast,
                        statements_handle,
                        Token::Symbol("c".to_owned()),
                        Token::Symbol("d".to_owned()),
                    );
                }

                // e = f;
                add_assignment_statement(
                    &mut expected_ast,
                    statements_handle,
                    Token::Symbol("e".to_owned()),
                    Token::Symbol("f".to_owned()),
                );
            }

            // expression at the end
            {
                let expression_handle = expected_ast
                    .add_child(brace_expression_handle, Rule::Expression);
                let brace_expression_handle = expected_ast
                    .add_child(expression_handle, Rule::BraceExpression);

                // statements
                {
                    let statements_handle = expected_ast.add_child(
                        brace_expression_handle,
                        Rule::BraceStatements,
                    );

                    // g = f;
                    add_assignment_statement(
                        &mut expected_ast,
                        statements_handle,
                        Token::Symbol("g".to_owned()),
                        Token::Symbol("f".to_owned()),
                    );
                }

                // expression at end
                add_terminal_expression(
                    &mut expected_ast,
                    brace_expression_handle,
                    Some(Token::Symbol("g".to_owned())),
                );
            }

            expected_ast
        };

        check_ast_equal(&ast, &expected_ast);
    }

    #[test]
    fn unary_expansion() {
        let tokens = tokenize("---1").expect("Unexpected tokenize error");
        let ast = parse(&tokens).expect("Unexpected parse errror");
        let expected_ast = {
            let mut expected_ast = Ast::new();
            let root_handle = expected_ast.add_root(Rule::Expression);

            let unary_handle = expected_ast.add_child_with_data(
                root_handle,
                Rule::Unary,
                Some(Token::Minus),
            );
            let unary_handle = expected_ast.add_child_with_data(
                unary_handle,
                Rule::Unary,
                Some(Token::Minus),
            );
            let unary_handle = expected_ast.add_child_with_data(
                unary_handle,
                Rule::Unary,
                Some(Token::Minus),
            );
            expected_ast
                .add_terminal_child(unary_handle, Some(Token::IntLiteral(1)));

            expected_ast
        };
        check_ast_equal(&ast, &expected_ast);
    }

    /// Because minus is a special operator (being both a binary and unary operator), it has
    /// some special case code for handling it, and thus this test case is important
    #[test]
    fn add_negative_number_lhs() {
        let tokens = tokenize("-1 + 1").expect("Unexpected tokenize error");
        let ast = parse(&tokens).expect("Unexpected parse errror");
        let expected_ast = {
            let mut expected_ast = Ast::new();

            let root_handle = expected_ast.add_root(Rule::Expression);

            let plus_minus_handle = expected_ast.add_child_with_data(
                root_handle,
                Rule::PlusMinus,
                Some(Token::Plus),
            );

            // LHS
            {
                let unary_handle = expected_ast.add_child_with_data(
                    plus_minus_handle,
                    Rule::Unary,
                    Some(Token::Minus),
                );
                expected_ast.add_terminal_child(
                    unary_handle,
                    Some(Token::IntLiteral(1)),
                );
            }

            // RHS
            expected_ast.add_terminal_child(
                plus_minus_handle,
                Some(Token::IntLiteral(1)),
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
        let ast = parse(&tokens).expect("Unexpected parse errror");
        let expected_ast = {
            let mut expected_ast = Ast::new();

            let root_handle = expected_ast.add_root(Rule::Expression);

            let plus_minus_handle = expected_ast.add_child_with_data(
                root_handle,
                Rule::PlusMinus,
                Some(Token::Plus),
            );

            // LHS
            expected_ast.add_terminal_child(
                plus_minus_handle,
                Some(Token::IntLiteral(1)),
            );

            // RHS
            {
                let unary_handle = expected_ast.add_child_with_data(
                    plus_minus_handle,
                    Rule::Unary,
                    Some(Token::Minus),
                );
                expected_ast.add_terminal_child(
                    unary_handle,
                    Some(Token::IntLiteral(1)),
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
        let ast = parse(&tokens).expect("Unexpected parse errror");
        let expected_ast = {
            let mut expected_ast = Ast::new();

            let root_handle = expected_ast.add_root(Rule::Expression);

            let plus_minus_handle = expected_ast.add_child_with_data(
                root_handle,
                Rule::PlusMinus,
                Some(Token::Plus),
            );

            // LHS
            {
                let unary_handle = expected_ast.add_child_with_data(
                    plus_minus_handle,
                    Rule::Unary,
                    Some(Token::Minus),
                );
                let unary_handle = expected_ast.add_child_with_data(
                    unary_handle,
                    Rule::Unary,
                    Some(Token::Minus),
                );
                expected_ast.add_terminal_child(
                    unary_handle,
                    Some(Token::IntLiteral(1)),
                );
            }

            // RHS
            expected_ast.add_terminal_child(
                plus_minus_handle,
                Some(Token::IntLiteral(1)),
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
        let ast = parse(&tokens).expect("Unexpected parse errror");
        let expected_ast = {
            let mut expected_ast = Ast::new();

            let root_handle = expected_ast.add_root(Rule::Expression);

            let plus_minus_handle = expected_ast.add_child_with_data(
                root_handle,
                Rule::PlusMinus,
                Some(Token::Plus),
            );

            // LHS
            expected_ast.add_terminal_child(
                plus_minus_handle,
                Some(Token::IntLiteral(1)),
            );

            // RHS
            {
                let unary_handle = expected_ast.add_child_with_data(
                    plus_minus_handle,
                    Rule::Unary,
                    Some(Token::Minus),
                );
                let unary_handle = expected_ast.add_child_with_data(
                    unary_handle,
                    Rule::Unary,
                    Some(Token::Minus),
                );
                expected_ast.add_terminal_child(
                    unary_handle,
                    Some(Token::IntLiteral(1)),
                );
            }

            expected_ast
        };

        check_ast_equal(&ast, &expected_ast);
    }

    /// Constructs expected ast for binary op and assignment composition
    fn binary_op_and_assign_expected_ast(rule: Rule, data: Token) -> Ast {
        let mut expected_ast = Ast::new();

        let root = expected_ast.add_root(Rule::Expression);
        let brace_expression =
            expected_ast.add_child(root, Rule::BraceExpression);

        let brace_statements =
            expected_ast.add_child(brace_expression, Rule::BraceStatements);
        {
            let statement =
                expected_ast.add_child(brace_statements, Rule::Statement);

            // LHS
            add_terminal_expression(
                &mut expected_ast,
                statement,
                Some(Token::Symbol("a".to_owned())),
            );

            // RHS
            let rhs = expected_ast.add_child(statement, Rule::Expression);
            let binary_op_node_handle =
                expected_ast.add_child_with_data(rhs, rule, Some(data));
            add_terminal_expression(
                &mut expected_ast,
                binary_op_node_handle,
                Some(Token::Symbol("a".to_owned())),
            );
            add_terminal_expression(
                &mut expected_ast,
                binary_op_node_handle,
                Some(Token::Symbol("b".to_owned())),
            );
        }

        add_terminal_expression(&mut expected_ast, brace_expression, None);

        expected_ast
    }

    #[test]
    fn binary_op_and_assign() {
        let tokens = tokenize("{a += b;}").expect("Unexpected tokenize error");
        let ast = parse(&tokens).expect("Unexpected parse errror");
        let expected_ast =
            binary_op_and_assign_expected_ast(Rule::PlusMinus, Token::Plus);

        check_ast_equal(&ast, &expected_ast);
    }

    #[test]
    fn binary_op_and_assign_minus() {
        let tokens = tokenize("{a -= b;}").expect("Unexpected tokenize error");
        let ast = parse(&tokens).expect("Unexpected parse errror");
        let expected_ast =
            binary_op_and_assign_expected_ast(Rule::PlusMinus, Token::Minus);

        check_ast_equal(&ast, &expected_ast);
    }

    #[test]
    fn binary_op_and_assign_times() {
        let tokens = tokenize("{a *= b;}").expect("Unexpected tokenize error");
        let ast = parse(&tokens).expect("Unexpected parse errror");
        let expected_ast =
            binary_op_and_assign_expected_ast(Rule::MultDiv, Token::Times);

        check_ast_equal(&ast, &expected_ast);
    }

    #[test]
    fn binary_op_and_assign_div() {
        let tokens = tokenize("{a /= b;}").expect("Unexpected tokenize error");
        let ast = parse(&tokens).expect("Unexpected parse errror");
        let expected_ast =
            binary_op_and_assign_expected_ast(Rule::MultDiv, Token::Divide);

        check_ast_equal(&ast, &expected_ast);
    }

    #[test]
    fn binary_op_and_assign_rhs_expression() {
        let tokens =
            tokenize("{a += b - c;}").expect("Unexpected tokenize error");
        let ast = parse(&tokens).expect("Unexpected parse errror");
        let expected_ast = {
            let mut expected_ast = Ast::new();

            let root = expected_ast.add_root(Rule::Expression);
            let brace_expression =
                expected_ast.add_child(root, Rule::BraceExpression);

            let brace_statements =
                expected_ast.add_child(brace_expression, Rule::BraceStatements);
            {
                let statement =
                    expected_ast.add_child(brace_statements, Rule::Statement);

                // LHS
                add_terminal_expression(
                    &mut expected_ast,
                    statement,
                    Some(Token::Symbol("a".to_owned())),
                );

                // RHS
                let rhs = expected_ast.add_child(statement, Rule::Expression);
                let plus_minus = expected_ast.add_child_with_data(
                    rhs,
                    Rule::PlusMinus,
                    Some(Token::Plus),
                );
                add_terminal_expression(
                    &mut expected_ast,
                    plus_minus,
                    Some(Token::Symbol("a".to_owned())),
                );

                // b - c
                {
                    let expression =
                        expected_ast.add_child(plus_minus, Rule::Expression);
                    let plus_minus = expected_ast.add_child_with_data(
                        expression,
                        Rule::PlusMinus,
                        Some(Token::Minus),
                    );

                    // b
                    expected_ast.add_terminal_child(
                        plus_minus,
                        Some(Token::Symbol("b".to_owned())),
                    );

                    // c
                    expected_ast.add_terminal_child(
                        plus_minus,
                        Some(Token::Symbol("c".to_owned())),
                    );
                }
            }

            add_terminal_expression(&mut expected_ast, brace_expression, None);

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
            }
        }
    }

    #[test]
    fn statement_lhs_is_expression() {
        let tokens = tokenize("{{a} = 1;}").expect("Unexpected tokenize error");
        let ast = parse(&tokens).expect("Unexpected parse errror");
        let expected_ast = {
            let mut expected_ast = Ast::new();
            let root_handle = expected_ast.add_root(Rule::Expression);
            let brace_expression =
                expected_ast.add_child(root_handle, Rule::BraceExpression);

            // statements
            {
                let brace_statements = expected_ast
                    .add_child(brace_expression, Rule::BraceStatements);
                let statement_handle =
                    expected_ast.add_child(brace_statements, Rule::Statement);

                // LHS
                {
                    let expression_handle = expected_ast
                        .add_child(statement_handle, Rule::Expression);
                    let brace_expression = expected_ast
                        .add_child(expression_handle, Rule::BraceExpression);
                    let expression_handle = expected_ast
                        .add_child(brace_expression, Rule::Expression);
                    expected_ast.add_terminal_child(
                        expression_handle,
                        Some(Token::Symbol("a".to_owned())),
                    );
                }

                // RHS
                add_terminal_expression(
                    &mut expected_ast,
                    statement_handle,
                    Some(Token::IntLiteral(1)),
                );
            }
            // expression
            add_terminal_expression(&mut expected_ast, brace_expression, None);
            expected_ast
        };

        check_ast_equal(&ast, &expected_ast);
    }

    #[test]
    fn statement_lhs_is_function() {
        let tokens =
            tokenize("{fun(a) = 1;}").expect("Unexpected tokenize error");
        let ast = parse(&tokens).expect("Unexpected parse errror");
        let expected_ast = {
            let mut expected_ast = Ast::new();
            let root_handle = expected_ast.add_root(Rule::Expression);
            let brace_expression =
                expected_ast.add_child(root_handle, Rule::BraceExpression);

            // statements
            {
                let brace_statements = expected_ast
                    .add_child(brace_expression, Rule::BraceStatements);
                let statement_handle =
                    expected_ast.add_child(brace_statements, Rule::Statement);

                // LHS
                {
                    let expression_handle = expected_ast
                        .add_child(statement_handle, Rule::Expression);
                    let fun_call_handle = expected_ast.add_child_with_data(
                        expression_handle,
                        Rule::FunctionCall,
                        Some(Token::Symbol("fun".to_owned())),
                    );
                    let fun_arg_handle = expected_ast
                        .add_child(fun_call_handle, Rule::FunctionArguments);
                    let expression_handle = expected_ast
                        .add_child(fun_arg_handle, Rule::Expression);
                    expected_ast.add_terminal_child(
                        expression_handle,
                        Some(Token::Symbol("a".to_owned())),
                    );
                }

                // RHS
                add_terminal_expression(
                    &mut expected_ast,
                    statement_handle,
                    Some(Token::IntLiteral(1)),
                );
            }
            // expression
            add_terminal_expression(&mut expected_ast, brace_expression, None);
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
            }
        }
    }

    fn add_function_call_no_arg(
        ast: &mut Ast,
        parent_handle: AstNodeHandle,
        function_name: String,
    ) {
        let function_call_handle = ast.add_child_with_data(
            parent_handle,
            Rule::FunctionCall,
            Some(Token::Symbol(function_name)),
        );
        let args_handle =
            ast.add_child(function_call_handle, Rule::FunctionArguments);
        add_terminal_expression(ast, args_handle, None);
    }

    #[test]
    fn function_call_no_arguments() {
        let tokens = tokenize("test()").expect("Unexpected tokenize error");
        let ast = parse(&tokens).expect("Unexpected parse errror");
        let expected_ast = {
            let mut expected_ast = Ast::new();
            let root_handle = expected_ast.add_root(Rule::Expression);

            add_function_call_no_arg(
                &mut expected_ast,
                root_handle,
                "test".to_owned(),
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
    ) {
        let function_call_handle = ast.add_child_with_data(
            parent_handle,
            Rule::FunctionCall,
            Some(Token::Symbol(function_name)),
        );
        let args_handle =
            ast.add_child(function_call_handle, Rule::FunctionArguments);
        add_terminal_expression(
            ast,
            args_handle,
            Some(Token::Symbol(arg_name)),
        );
    }

    #[test]
    fn function_call_one_argument() {
        let tokens = tokenize("test(me)").expect("Unexpected tokenize error");
        let ast = parse(&tokens).expect("Unexpected parse errror");

        let expected_ast = {
            let mut expected_ast = Ast::new();
            let root_handle = expected_ast.add_root(Rule::Expression);
            add_function_call_one_arg(
                &mut expected_ast,
                root_handle,
                "test".to_owned(),
                "me".to_owned(),
            );

            expected_ast
        };
        check_ast_equal(&ast, &expected_ast);
    }

    #[test]
    fn function_call_two_arguments() {
        let tokens =
            tokenize("test(me, please)").expect("Unexpected tokenize error");
        let ast = parse(&tokens).expect("Unexpected parse errror");
        let expected_ast = {
            let mut expected_ast = Ast::new();
            let root_handle = expected_ast.add_root(Rule::Expression);
            let function_call_handle = expected_ast.add_child_with_data(
                root_handle,
                Rule::FunctionCall,
                Some(Token::Symbol("test".to_owned())),
            );
            let args_handle = expected_ast
                .add_child(function_call_handle, Rule::FunctionArguments);

            // "me" argument
            {
                // recursive arg
                let args_handle = expected_ast
                    .add_child(args_handle, Rule::FunctionArguments);
                add_terminal_expression(
                    &mut expected_ast,
                    args_handle,
                    Some(Token::Symbol("me".to_owned())),
                );
            }

            // "please" argument
            add_terminal_expression(
                &mut expected_ast,
                args_handle,
                Some(Token::Symbol("please".to_owned())),
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
    ) {
        let function_call_handle = ast.add_child_with_data(
            parent_handle,
            Rule::FunctionCall,
            Some(Token::Symbol(function_name)),
        );
        let args_handle =
            ast.add_child(function_call_handle, Rule::FunctionArguments);

        // arg 1 and 2
        {
            // recursive arg
            let args_handle =
                ast.add_child(args_handle, Rule::FunctionArguments);

            {
                let args_handle =
                    ast.add_child(args_handle, Rule::FunctionArguments);
                add_terminal_expression(
                    ast,
                    args_handle,
                    Some(Token::Symbol(arg1_name)),
                );
            }
            add_terminal_expression(
                ast,
                args_handle,
                Some(Token::Symbol(arg2_name)),
            );
        }

        // arg 3
        add_terminal_expression(
            ast,
            args_handle,
            Some(Token::Symbol(arg3_name)),
        );
    }

    #[test]
    fn function_call_multiple_arguments() {
        let tokens = tokenize("test(me, please, thanks)")
            .expect("Unexpected tokenize error");
        let ast = parse(&tokens).expect("Unexpected parse errror");
        let expected_ast = {
            let mut expected_ast = Ast::new();
            let root_handle = expected_ast.add_root(Rule::Expression);
            add_function_call_multiple_arguments(
                &mut expected_ast,
                root_handle,
                "test".to_owned(),
                "me".to_owned(),
                "please".to_owned(),
                "thanks".to_owned(),
            );

            expected_ast
        };
        check_ast_equal(&ast, &expected_ast);
    }

    #[test]
    fn function_call_trailing_comma_single_arg() {
        let tokens = tokenize("test(me,)").expect("Unexpected tokenize error");
        let ast = parse(&tokens).expect("Unexpected parse errror");
        let expected_ast = {
            let mut expected_ast = Ast::new();
            let root_handle = expected_ast.add_root(Rule::Expression);
            add_function_call_one_arg(
                &mut expected_ast,
                root_handle,
                "test".to_owned(),
                "me".to_owned(),
            );
            expected_ast
        };
        check_ast_equal(&ast, &expected_ast);
    }

    #[test]
    fn function_call_trailing_comma() {
        let tokens = tokenize("test(me, please, thanks,)")
            .expect("Unexpected tokenize error");
        let ast = parse(&tokens).expect("Unexpected parse errror");
        let expected_ast = {
            let mut expected_ast = Ast::new();
            let root_handle = expected_ast.add_root(Rule::Expression);
            add_function_call_multiple_arguments(
                &mut expected_ast,
                root_handle,
                "test".to_owned(),
                "me".to_owned(),
                "please".to_owned(),
                "thanks".to_owned(),
            );
            expected_ast
        };

        check_ast_equal(&ast, &expected_ast);
    }

    #[test]
    fn function_calls_nested() {
        let tokens =
            tokenize("test(first_inner(a, b, c,), second_inner(d(e),))")
                .expect("Unexpected tokenize error");

        let ast = parse(&tokens).expect("Unexpected parse errror");
        let expected_ast = {
            let mut expected_ast = Ast::new();
            let root_handle = expected_ast.add_root(Rule::Expression);
            let function_call_handle = expected_ast.add_child_with_data(
                root_handle,
                Rule::FunctionCall,
                Some(Token::Symbol("test".to_owned())),
            );
            let args_handle = expected_ast
                .add_child(function_call_handle, Rule::FunctionArguments);

            // arg 1
            {
                let args_handle = expected_ast
                    .add_child(args_handle, Rule::FunctionArguments);
                let expression_handle =
                    expected_ast.add_child(args_handle, Rule::Expression);
                add_function_call_multiple_arguments(
                    &mut expected_ast,
                    expression_handle,
                    "first_inner".to_owned(),
                    "a".to_owned(),
                    "b".to_owned(),
                    "c".to_owned(),
                );
            }

            // arg 2
            {
                let expression_handle =
                    expected_ast.add_child(args_handle, Rule::Expression);
                let function_call_handle = expected_ast.add_child_with_data(
                    expression_handle,
                    Rule::FunctionCall,
                    Some(Token::Symbol("second_inner".to_owned())),
                );
                let args_handle = expected_ast
                    .add_child(function_call_handle, Rule::FunctionArguments);
                let expression_handle =
                    expected_ast.add_child(args_handle, Rule::Expression);
                add_function_call_one_arg(
                    &mut expected_ast,
                    expression_handle,
                    "d".to_owned(),
                    "e".to_owned(),
                );
            }
            expected_ast
        };
        check_ast_equal(&ast, &expected_ast);
    }

    #[test]
    fn function_call_argument_expression() {
        let tokens = tokenize("test(1 + 2, please,)")
            .expect("Unexpected tokenize error");
        let ast = parse(&tokens).expect("Unexpected parse errror");
        let expected_ast = {
            let mut expected_ast = Ast::new();
            let root_handle = expected_ast.add_root(Rule::Expression);
            let function_call_handle = expected_ast.add_child_with_data(
                root_handle,
                Rule::FunctionCall,
                Some(Token::Symbol("test".to_owned())),
            );
            let args_handle = expected_ast
                .add_child(function_call_handle, Rule::FunctionArguments);

            // 1 + 2 argument
            {
                // recursive arg
                let args_handle = expected_ast
                    .add_child(args_handle, Rule::FunctionArguments);

                // 1 + 2
                let expression_handle =
                    expected_ast.add_child(args_handle, Rule::Expression);
                add_expected_add_child(
                    &mut expected_ast,
                    expression_handle,
                    Token::IntLiteral(1),
                    Token::IntLiteral(2),
                );
            }

            // "please" argument
            add_terminal_expression(
                &mut expected_ast,
                args_handle,
                Some(Token::Symbol("please".to_owned())),
            );

            expected_ast
        };
        check_ast_equal(&ast, &expected_ast);
    }

    #[test]
    fn function_call_parens_in_expression() {
        let tokens = tokenize("test((1 + 2), please,)")
            .expect("Unexpected tokenize error");
        let ast = parse(&tokens).expect("Unexpected parse errror");
        let expected_ast = {
            let mut expected_ast = Ast::new();
            let root_handle = expected_ast.add_root(Rule::Expression);
            let function_call_handle = expected_ast.add_child_with_data(
                root_handle,
                Rule::FunctionCall,
                Some(Token::Symbol("test".to_owned())),
            );
            let args_handle = expected_ast
                .add_child(function_call_handle, Rule::FunctionArguments);

            // (1 + 2) argument
            {
                // recursive arg
                let args_handle = expected_ast
                    .add_child(args_handle, Rule::FunctionArguments);

                // (1 + 2)
                let expression_handle =
                    expected_ast.add_child(args_handle, Rule::Expression);
                let expression_handle =
                    expected_ast.add_child(expression_handle, Rule::Expression);
                // 1 + 2
                add_expected_add_child(
                    &mut expected_ast,
                    expression_handle,
                    Token::IntLiteral(1),
                    Token::IntLiteral(2),
                );
            }

            // "please" argument
            add_terminal_expression(
                &mut expected_ast,
                args_handle,
                Some(Token::Symbol("please".to_owned())),
            );

            expected_ast
        };
        check_ast_equal(&ast, &expected_ast);
    }

    #[test]
    fn function_call_braced_expression() {
        let tokens = tokenize("test({1 + 2}, please,)")
            .expect("Unexpected tokenize error");
        let ast = parse(&tokens).expect("Unexpected parse errror");
        let expected_ast = {
            let mut expected_ast = Ast::new();
            let root_handle = expected_ast.add_root(Rule::Expression);
            let function_call_handle = expected_ast.add_child_with_data(
                root_handle,
                Rule::FunctionCall,
                Some(Token::Symbol("test".to_owned())),
            );
            let args_handle = expected_ast
                .add_child(function_call_handle, Rule::FunctionArguments);

            // {1 + 2} argument
            {
                // recursive arg
                let args_handle = expected_ast
                    .add_child(args_handle, Rule::FunctionArguments);

                // {1 + 2}
                let expression_handle =
                    expected_ast.add_child(args_handle, Rule::Expression);
                let brace_expression_handle = expected_ast
                    .add_child(expression_handle, Rule::BraceExpression);
                let expression_handle = expected_ast
                    .add_child(brace_expression_handle, Rule::Expression);
                // 1 + 2
                add_expected_add_child(
                    &mut expected_ast,
                    expression_handle,
                    Token::IntLiteral(1),
                    Token::IntLiteral(2),
                );
            }

            // "please" argument
            add_terminal_expression(
                &mut expected_ast,
                args_handle,
                Some(Token::Symbol("please".to_owned())),
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
        let ast = parse(&tokens).expect("Unexpected parse errror");
        let expected_ast = {
            let mut expected_ast = Ast::new();
            let root_handle = expected_ast.add_root(Rule::Expression);
            let brace_expression_handle =
                expected_ast.add_child(root_handle, Rule::BraceExpression);
            let brace_statements_handle = expected_ast
                .add_child(brace_expression_handle, Rule::BraceStatements);

            // fun_a()
            {
                // recursive call
                let brace_statements_handle: AstNodeHandle = expected_ast
                    .add_child(brace_statements_handle, Rule::BraceStatements);
                let statement_handle = expected_ast
                    .add_child(brace_statements_handle, Rule::Statement);

                // LHS
                add_terminal_expression(
                    &mut expected_ast,
                    statement_handle,
                    Some(Token::Symbol("a".to_owned())),
                );

                // RHS
                {
                    let expression_handle = expected_ast
                        .add_child(statement_handle, Rule::Expression);
                    add_function_call_no_arg(
                        &mut expected_ast,
                        expression_handle,
                        "fun_a".to_owned(),
                    );
                }
            }

            // fun_b(b)
            {
                let statement_handle = expected_ast
                    .add_child(brace_statements_handle, Rule::Statement);
                let expression_handle =
                    expected_ast.add_child(statement_handle, Rule::Expression);
                add_function_call_one_arg(
                    &mut expected_ast,
                    expression_handle,
                    "fun_b".to_owned(),
                    "b".to_owned(),
                );
            }

            // return expression
            {
                let expression_handle = expected_ast
                    .add_child(brace_expression_handle, Rule::Expression);
                add_function_call_multiple_arguments(
                    &mut expected_ast,
                    expression_handle,
                    "fun_c".to_owned(),
                    "a".to_owned(),
                    "b".to_owned(),
                    "c".to_owned(),
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
        let ast = parse(&tokens).expect("Unexpected parse errror");
        let expected_ast = {
            let mut expected_ast = Ast::new();
            let root_handle = expected_ast.add_root(Rule::Expression);
            let function_call_handle = expected_ast.add_child_with_data(
                root_handle,
                Rule::FunctionCall,
                Some(Token::Symbol("test".to_owned())),
            );

            let function_arguments_handle = expected_ast
                .add_child(function_call_handle, Rule::FunctionArguments);
            let expression_handle = expected_ast
                .add_child(function_arguments_handle, Rule::Expression);
            expected_ast.add_terminal_child(expression_handle, None);

            expected_ast
        };
        check_ast_equal(&ast, &expected_ast);
    }

    #[test]
    fn empty_function() {
        let tokens =
            tokenize("fn test() {}").expect("Unexpected tokenize error");
        let ast = parse(&tokens).expect("Unexpected parse errror");

        let expected_ast = {
            let mut expected_ast = Ast::new();

            let root_handle = expected_ast.add_root(Rule::Expression);
            let function_def_handle = expected_ast.add_child_with_data(
                root_handle,
                Rule::FunctionDef,
                Some(Token::Symbol("test".to_owned())),
            );

            // no parameters, so this has no children
            expected_ast
                .add_child(function_def_handle, Rule::FunctionDefParameters);

            // brace expression
            let brace_expression_handle = expected_ast
                .add_child(function_def_handle, Rule::BraceExpression);
            add_terminal_expression(
                &mut expected_ast,
                brace_expression_handle,
                None,
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
        let ast = parse(&tokens).expect("Unexpected parse errror");

        let expected_ast = {
            let mut expected_ast = Ast::new();

            let param_one = "a".to_owned();
            let param_two = "b".to_owned();

            let root_handle = expected_ast.add_root(Rule::Expression);

            let function_def_handle = add_basic_function_declaration(
                &mut expected_ast,
                root_handle,
                &param_one,
                &param_two,
            );

            // brace expression
            let brace_expression_handle = expected_ast
                .add_child(function_def_handle, Rule::BraceExpression);

            // brace statements
            let brace_statements = expected_ast
                .add_child(brace_expression_handle, Rule::BraceStatements);
            let statement_handle =
                expected_ast.add_child(brace_statements, Rule::Statement);
            let expression_handle =
                expected_ast.add_child(statement_handle, Rule::Expression);
            add_basic_for_loop(&mut expected_ast, expression_handle);

            add_terminal_expression(
                &mut expected_ast,
                brace_expression_handle,
                Some(Token::Symbol("b".to_owned())),
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
        let ast = parse(&tokens).expect("Unexpected parse errror");

        let expected_ast = {
            let mut expected_ast = Ast::new();

            let param_one = "a".to_owned();
            let param_two = "b".to_owned();

            let root_handle = expected_ast.add_root(Rule::Expression);

            let function_def_handle = add_basic_function_declaration(
                &mut expected_ast,
                root_handle,
                &param_one,
                &param_two,
            );

            // brace expression
            let brace_expression_handle = expected_ast
                .add_child(function_def_handle, Rule::BraceExpression);

            // trailing expression
            let expression_handle = expected_ast
                .add_child(brace_expression_handle, Rule::Expression);
            add_basic_for_loop(&mut expected_ast, expression_handle);

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
        let ast = parse(&tokens).expect("Unexpected parse errror");

        let expected_ast = {
            let mut expected_ast = Ast::new();

            let root_handle = expected_ast.add_root(Rule::Expression);
            let function_def_handle = expected_ast.add_child_with_data(
                root_handle,
                Rule::FunctionDef,
                Some(Token::Symbol("test".to_owned())),
            );

            // no parameters, so this has no children
            expected_ast
                .add_child(function_def_handle, Rule::FunctionDefParameters);

            // brace expression
            let brace_expression_handle = expected_ast
                .add_child(function_def_handle, Rule::BraceExpression);
            let expression_handle = expected_ast
                .add_child(brace_expression_handle, Rule::Expression);
            add_expected_add_child(
                &mut expected_ast,
                expression_handle,
                Token::Symbol("a".to_owned()),
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
    ) {
        let function_def_handle = ast.add_child_with_data(
            parent_handle,
            Rule::FunctionDef,
            Some(Token::Symbol(function_name)),
        );

        // parameters
        {
            let function_parameters_handle =
                ast.add_child(function_def_handle, Rule::FunctionDefParameters);

            // recursive side
            ast.add_child(
                function_parameters_handle,
                Rule::FunctionDefParameters,
            );

            // non-recursive side
            {
                let declaration_handle = ast
                    .add_child(function_parameters_handle, Rule::Declaration);
                ast.add_terminal_child(
                    declaration_handle,
                    Some(Token::Symbol(param_type)),
                );
                ast.add_terminal_child(
                    declaration_handle,
                    Some(Token::Symbol(param_name.clone())),
                );
            }
        }

        let brace_expression_handle =
            ast.add_child(function_def_handle, Rule::BraceExpression);
        let expression_handle =
            ast.add_child(brace_expression_handle, Rule::Expression);
        add_expected_add_child(
            ast,
            expression_handle,
            Token::Symbol(param_name.clone()),
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
        let ast = parse(&tokens).expect("Unexpected parse errror");

        let expected_ast = {
            let mut expected_ast = Ast::new();

            let root_handle = expected_ast.add_root(Rule::Expression);
            add_one_param_function(
                &mut expected_ast,
                root_handle,
                "test".to_owned(),
                "int32".to_owned(),
                "a".to_owned(),
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
    ) {
        let function_def_handle = ast.add_child_with_data(
            parent_handle,
            Rule::FunctionDef,
            Some(Token::Symbol(function_name)),
        );

        // parameters
        {
            let function_parameters_handle =
                ast.add_child(function_def_handle, Rule::FunctionDefParameters);

            // recursive side
            {
                let function_parameters_handle = ast.add_child(
                    function_parameters_handle,
                    Rule::FunctionDefParameters,
                );

                // recursive side
                ast.add_child(
                    function_parameters_handle,
                    Rule::FunctionDefParameters,
                );

                // non-recursive side
                let declaration_handle = ast
                    .add_child(function_parameters_handle, Rule::Declaration);
                ast.add_terminal_child(
                    declaration_handle,
                    Some(Token::Symbol(param1_type)),
                );
                ast.add_terminal_child(
                    declaration_handle,
                    Some(Token::Symbol(param1_name.clone())),
                );
            }

            // non-recursive side
            {
                let declaration_handle = ast
                    .add_child(function_parameters_handle, Rule::Declaration);
                ast.add_terminal_child(
                    declaration_handle,
                    Some(Token::Symbol(param2_type.clone())),
                );
                ast.add_terminal_child(
                    declaration_handle,
                    Some(Token::Symbol(param2_name.clone())),
                );
            }
        }

        let brace_expression_handle =
            ast.add_child(function_def_handle, Rule::BraceExpression);
        let expression_handle =
            ast.add_child(brace_expression_handle, Rule::Expression);
        add_expected_add_child(
            ast,
            expression_handle,
            Token::Symbol(param1_name.clone()),
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
        let ast = parse(&tokens).expect("Unexpected parse errror");

        let expected_ast = {
            let mut expected_ast = Ast::new();

            let root_handle = expected_ast.add_root(Rule::Expression);
            add_two_param_function(
                &mut expected_ast,
                root_handle,
                "test".to_owned(),
                "int32".to_owned(),
                "a".to_owned(),
                "int32".to_owned(),
                "b".to_owned(),
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
        let ast = parse(&tokens).expect("Unexpected parse errror");

        let expected_ast = {
            let mut expected_ast = Ast::new();

            let root_handle = expected_ast.add_root(Rule::Expression);
            add_one_param_function(
                &mut expected_ast,
                root_handle,
                "test".to_owned(),
                "int32".to_owned(),
                "a".to_owned(),
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
        let ast = parse(&tokens).expect("Unexpected parse errror");

        let expected_ast = {
            let mut expected_ast = Ast::new();

            let root_handle = expected_ast.add_root(Rule::Expression);
            add_two_param_function(
                &mut expected_ast,
                root_handle,
                "test".to_owned(),
                "int32".to_owned(),
                "a".to_owned(),
                "float32".to_owned(),
                "b".to_owned(),
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
            }
        }
    }

    /// Adds the tree to the AST for a function declaration with two arguments
    fn add_basic_function_declaration(
        ast: &mut Ast,
        parent_handle: AstNodeHandle,
        param1_name: &String,
        param2_name: &String,
    ) -> AstNodeHandle {
        let function_def_handle = ast.add_child_with_data(
            parent_handle,
            Rule::FunctionDef,
            Some(Token::Symbol("test".to_owned())),
        );

        // parameters
        {
            let function_parameters_handle =
                ast.add_child(function_def_handle, Rule::FunctionDefParameters);

            // recursive side
            {
                let function_parameters_handle = ast.add_child(
                    function_parameters_handle,
                    Rule::FunctionDefParameters,
                );

                // recursive side
                ast.add_child(
                    function_parameters_handle,
                    Rule::FunctionDefParameters,
                );

                // non-recursive side
                let declaration_handle = ast
                    .add_child(function_parameters_handle, Rule::Declaration);
                ast.add_terminal_child(
                    declaration_handle,
                    Some(Token::Symbol("int32".to_owned())),
                );
                ast.add_terminal_child(
                    declaration_handle,
                    Some(Token::Symbol(param1_name.clone())),
                );
            }

            // non-recursive side
            {
                let declaration_handle = ast
                    .add_child(function_parameters_handle, Rule::Declaration);
                ast.add_terminal_child(
                    declaration_handle,
                    Some(Token::Symbol("int32".to_owned())),
                );
                ast.add_terminal_child(
                    declaration_handle,
                    Some(Token::Symbol(param2_name.clone())),
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
        let ast = parse(&tokens).expect("Unexpected parse errror");
        let expected_ast = {
            let param1_name = "a".to_owned();
            let param2_name = "b".to_owned();

            let mut expected_ast = Ast::new();
            let root_handle = expected_ast.add_root(Rule::Expression);

            let function_def_handle = add_basic_function_declaration(
                &mut expected_ast,
                root_handle,
                &param1_name,
                &param2_name,
            );

            let brace_expression_handle = expected_ast
                .add_child(function_def_handle, Rule::BraceExpression);

            // brace statements
            {
                let brace_statements = expected_ast
                    .add_child(brace_expression_handle, Rule::BraceStatements);
                let return_statement = expected_ast
                    .add_child(brace_statements, Rule::ReturnStatement);
                let return_expression =
                    expected_ast.add_child(return_statement, Rule::Expression);
                add_expected_add_child(
                    &mut expected_ast,
                    return_expression,
                    Token::Symbol(param1_name.clone()),
                    Token::Symbol(param2_name.clone()),
                );
            }

            add_terminal_expression(
                &mut expected_ast,
                brace_expression_handle,
                None,
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

        let ast = parse(&tokens).expect("Unexpected parse errror");
        let expected_ast = {
            let param1_name = "a".to_owned();
            let param2_name = "b".to_owned();

            let mut expected_ast = Ast::new();
            let root_handle = expected_ast.add_root(Rule::Expression);

            let function_def_handle = add_basic_function_declaration(
                &mut expected_ast,
                root_handle,
                &param1_name,
                &param2_name,
            );

            let brace_expression_handle = expected_ast
                .add_child(function_def_handle, Rule::BraceExpression);

            // brace statements
            {
                let brace_statements = expected_ast
                    .add_child(brace_expression_handle, Rule::BraceStatements);

                let statement_handle =
                    expected_ast.add_child(brace_statements, Rule::Statement);
                let expression_handle =
                    expected_ast.add_child(statement_handle, Rule::Expression);
                let if_else_handle =
                    expected_ast.add_child(expression_handle, Rule::IfElse);

                // condition
                {
                    let condition_expression_handle = expected_ast
                        .add_child(if_else_handle, Rule::Expression);
                    let equality_handle = expected_ast.add_child_with_data(
                        condition_expression_handle,
                        Rule::Comparison,
                        Some(Token::GreaterThan),
                    );

                    // LHS
                    expected_ast.add_child_with_data(
                        equality_handle,
                        Rule::Terminal,
                        Some(Token::Symbol("a".to_owned())),
                    );

                    // RHS
                    expected_ast.add_child_with_data(
                        equality_handle,
                        Rule::Terminal,
                        Some(Token::IntLiteral(0)),
                    );
                }

                // if side
                {
                    let brace_expression_handle = expected_ast
                        .add_child(if_else_handle, Rule::BraceExpression);
                    let brace_statements_handle = expected_ast.add_child(
                        brace_expression_handle,
                        Rule::BraceStatements,
                    );
                    let return_statement_handle = expected_ast.add_child(
                        brace_statements_handle,
                        Rule::ReturnStatement,
                    );
                    add_terminal_expression(
                        &mut expected_ast,
                        return_statement_handle,
                        Some(Token::Symbol("a".to_owned())),
                    );

                    add_terminal_expression(
                        &mut expected_ast,
                        brace_expression_handle,
                        None,
                    );
                }

                // else side
                {
                    let expression_handle = expected_ast
                        .add_child(if_else_handle, Rule::Expression);
                    let brace_expression_handle = expected_ast
                        .add_child(expression_handle, Rule::BraceExpression);
                    let brace_statements_handle = expected_ast.add_child(
                        brace_expression_handle,
                        Rule::BraceStatements,
                    );
                    let return_statement_handle = expected_ast.add_child(
                        brace_statements_handle,
                        Rule::ReturnStatement,
                    );
                    add_terminal_expression(
                        &mut expected_ast,
                        return_statement_handle,
                        Some(Token::Symbol("b".to_owned())),
                    );

                    add_terminal_expression(
                        &mut expected_ast,
                        brace_expression_handle,
                        None,
                    );
                }
            }

            add_terminal_expression(
                &mut expected_ast,
                brace_expression_handle,
                None,
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

        let ast = parse(&tokens).expect("Unexpected parse errror");
        let expected_ast = {
            let param1_name = "a".to_owned();
            let param2_name = "b".to_owned();

            let mut expected_ast = Ast::new();
            let root_handle = expected_ast.add_root(Rule::Expression);

            let function_def_handle = add_basic_function_declaration(
                &mut expected_ast,
                root_handle,
                &param1_name,
                &param2_name,
            );

            let brace_expression_handle = expected_ast
                .add_child(function_def_handle, Rule::BraceExpression);

            let expression_handle = expected_ast
                .add_child(brace_expression_handle, Rule::Expression);
            let if_else_handle =
                expected_ast.add_child(expression_handle, Rule::IfElse);

            // condition
            {
                let condition_expression_handle =
                    expected_ast.add_child(if_else_handle, Rule::Expression);

                // b/c of the parens, we actually have two expressions in a row
                let condition_expression_handle = expected_ast
                    .add_child(condition_expression_handle, Rule::Expression);

                let equality_handle = expected_ast.add_child_with_data(
                    condition_expression_handle,
                    Rule::Comparison,
                    Some(Token::GreaterThan),
                );

                // LHS
                expected_ast.add_child_with_data(
                    equality_handle,
                    Rule::Terminal,
                    Some(Token::Symbol("a".to_owned())),
                );

                // RHS
                expected_ast.add_child_with_data(
                    equality_handle,
                    Rule::Terminal,
                    Some(Token::IntLiteral(0)),
                );
            }

            // if side
            {
                let brace_expression_handle = expected_ast
                    .add_child(if_else_handle, Rule::BraceExpression);
                add_terminal_expression(
                    &mut expected_ast,
                    brace_expression_handle,
                    Some(Token::Symbol("a".to_owned())),
                );
            }

            // else side
            {
                let expression_handle =
                    expected_ast.add_child(if_else_handle, Rule::Expression);
                let brace_expression_handle = expected_ast
                    .add_child(expression_handle, Rule::BraceExpression);

                add_terminal_expression(
                    &mut expected_ast,
                    brace_expression_handle,
                    Some(Token::Symbol("b".to_owned())),
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

        let ast = parse(&tokens).expect("Unexpected parse errror");
        let expected_ast = {
            let param1_name = "a".to_owned();
            let param2_name = "b".to_owned();

            let mut expected_ast = Ast::new();
            let root_handle = expected_ast.add_root(Rule::Expression);

            let function_def_handle = add_basic_function_declaration(
                &mut expected_ast,
                root_handle,
                &param1_name,
                &param2_name,
            );

            let brace_expression_handle = expected_ast
                .add_child(function_def_handle, Rule::BraceExpression);

            // brace statements
            {
                let brace_statements = expected_ast
                    .add_child(brace_expression_handle, Rule::BraceStatements);

                // first statement
                {
                    let brace_statements = expected_ast
                        .add_child(brace_statements, Rule::BraceStatements);
                    let statement_handle = expected_ast
                        .add_child(brace_statements, Rule::Statement);
                    let expression_handle = expected_ast
                        .add_child(statement_handle, Rule::Expression);
                    let if_else_handle =
                        expected_ast.add_child(expression_handle, Rule::IfElse);

                    // condition
                    {
                        let condition_expression_handle = expected_ast
                            .add_child(if_else_handle, Rule::Expression);
                        let equality_handle = expected_ast.add_child_with_data(
                            condition_expression_handle,
                            Rule::Comparison,
                            Some(Token::GreaterThan),
                        );

                        // LHS
                        expected_ast.add_child_with_data(
                            equality_handle,
                            Rule::Terminal,
                            Some(Token::Symbol("a".to_owned())),
                        );

                        // RHS
                        expected_ast.add_child_with_data(
                            equality_handle,
                            Rule::Terminal,
                            Some(Token::IntLiteral(0)),
                        );
                    }

                    // if side
                    {
                        let brace_expression_handle = expected_ast
                            .add_child(if_else_handle, Rule::BraceExpression);
                        let brace_statements_handle = expected_ast.add_child(
                            brace_expression_handle,
                            Rule::BraceStatements,
                        );
                        let return_statement_handle = expected_ast.add_child(
                            brace_statements_handle,
                            Rule::ReturnStatement,
                        );
                        add_terminal_expression(
                            &mut expected_ast,
                            return_statement_handle,
                            Some(Token::Symbol("a".to_owned())),
                        );

                        add_terminal_expression(
                            &mut expected_ast,
                            brace_expression_handle,
                            None,
                        );
                    }
                }

                // second statement
                {
                    let return_statement_handle = expected_ast
                        .add_child(brace_statements, Rule::ReturnStatement);
                    add_terminal_expression(
                        &mut expected_ast,
                        return_statement_handle,
                        Some(Token::Symbol("b".to_owned())),
                    );
                }
            }

            add_terminal_expression(
                &mut expected_ast,
                brace_expression_handle,
                None,
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

        let ast = parse(&tokens).expect("Unexpected parse errror");
        let expected_ast = {
            let param1_name = "a".to_owned();
            let param2_name = "b".to_owned();

            let mut expected_ast = Ast::new();
            let root_handle = expected_ast.add_root(Rule::Expression);

            let function_def_handle = add_basic_function_declaration(
                &mut expected_ast,
                root_handle,
                &param1_name,
                &param2_name,
            );

            let brace_expression_handle = expected_ast
                .add_child(function_def_handle, Rule::BraceExpression);

            // brace statements
            {
                let brace_statements = expected_ast
                    .add_child(brace_expression_handle, Rule::BraceStatements);

                // first statement
                {
                    let statement_handle = expected_ast
                        .add_child(brace_statements, Rule::Statement);
                    let expression_handle = expected_ast
                        .add_child(statement_handle, Rule::Expression);
                    let if_else_handle =
                        expected_ast.add_child(expression_handle, Rule::IfElse);

                    // condition
                    {
                        let condition_expression_handle = expected_ast
                            .add_child(if_else_handle, Rule::Expression);
                        let equality_handle = expected_ast.add_child_with_data(
                            condition_expression_handle,
                            Rule::Comparison,
                            Some(Token::GreaterThan),
                        );

                        // LHS
                        expected_ast.add_child_with_data(
                            equality_handle,
                            Rule::Terminal,
                            Some(Token::Symbol("a".to_owned())),
                        );

                        // RHS
                        expected_ast.add_child_with_data(
                            equality_handle,
                            Rule::Terminal,
                            Some(Token::IntLiteral(0)),
                        );
                    }

                    // if side
                    {
                        let brace_expression_handle = expected_ast
                            .add_child(if_else_handle, Rule::BraceExpression);
                        let brace_statements_handle = expected_ast.add_child(
                            brace_expression_handle,
                            Rule::BraceStatements,
                        );
                        let return_statement_handle = expected_ast.add_child(
                            brace_statements_handle,
                            Rule::ReturnStatement,
                        );
                        add_terminal_expression(
                            &mut expected_ast,
                            return_statement_handle,
                            Some(Token::Symbol("a".to_owned())),
                        );

                        add_terminal_expression(
                            &mut expected_ast,
                            brace_expression_handle,
                            None,
                        );
                    }
                }
            }

            add_terminal_expression(
                &mut expected_ast,
                brace_expression_handle,
                Some(Token::Symbol("b".to_owned())),
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
                // TODO: check line number
                // TODO: check error info?
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
            let param1_name = "a".to_owned();
            let param2_name = "b".to_owned();

            let mut expected_ast = Ast::new();

            let root_handle = expected_ast.add_root(Rule::Expression);

            let function_def_handle = add_basic_function_declaration(
                &mut expected_ast,
                root_handle,
                &param1_name,
                &param2_name,
            );

            let brace_expression_handle = expected_ast
                .add_child(function_def_handle, Rule::BraceExpression);

            // statements
            {
                let brace_statements = expected_ast
                    .add_child(brace_expression_handle, Rule::BraceStatements);
                let statement_handle =
                    expected_ast.add_child(brace_statements, Rule::Statement);
                let expression_handle =
                    expected_ast.add_child(statement_handle, Rule::Expression);

                let for_handle = add_for_loop_declaration(
                    &mut expected_ast,
                    expression_handle,
                );
                // for loop brace_expression
                {
                    let brace_expression = expected_ast
                        .add_child(for_handle, Rule::BraceExpression);
                    // brace statements
                    {
                        let brace_statements = expected_ast
                            .add_child(brace_expression, Rule::BraceStatements);
                        let return_statement_handle = expected_ast
                            .add_child(brace_statements, Rule::ReturnStatement);

                        add_terminal_expression(
                            &mut expected_ast,
                            return_statement_handle,
                            Some(Token::Symbol("b".to_owned())),
                        );
                    }

                    // expression
                    add_terminal_expression(
                        &mut expected_ast,
                        brace_expression,
                        None,
                    );
                }
            }

            add_terminal_expression(
                &mut expected_ast,
                brace_expression_handle,
                None,
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
        let ast = match parse(&tokens) {
            Ok(_) => assert!(false),
            Err(errors) => {
                assert_eq!(errors.len(), 1);
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
            let param1_name = "a".to_owned();
            let param2_name = "b".to_owned();

            let mut expected_ast = Ast::new();
            let root_handle = expected_ast.add_root(Rule::Expression);

            let function_def_handle = add_basic_function_declaration(
                &mut expected_ast,
                root_handle,
                &param1_name,
                &param2_name,
            );

            let brace_expression_handle = expected_ast
                .add_child(function_def_handle, Rule::BraceExpression);

            // brace statements
            {
                let brace_statements = expected_ast
                    .add_child(brace_expression_handle, Rule::BraceStatements);

                // first statement
                {
                    let return_statement_handle = expected_ast
                        .add_child(brace_statements, Rule::ReturnStatement);
                    add_terminal_expression(
                        &mut expected_ast,
                        return_statement_handle,
                        Some(Token::Symbol("a".to_owned())),
                    );
                }
            }

            add_terminal_expression(
                &mut expected_ast,
                brace_expression_handle,
                Some(Token::Symbol("b".to_owned())),
            );

            expected_ast
        };

        check_ast_equal(&ast, &expected_ast);
    }
}
