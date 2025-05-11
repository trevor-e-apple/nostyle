use std::fs::File;
use std::path::Path;
use std::println;
use std::time::SystemTime;

use crate::parse::ast::get_diff_string;
use crate::parse::*;

/// helper function for adding an expression with nothing but a terminal
/// to an ast
pub fn add_terminal_expression(
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
pub fn add_expected_add_child(
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
pub fn add_expected_mult_child(
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
pub fn add_assignment_statement(
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
pub fn add_no_statements(
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
pub fn add_comparison_tree(
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

pub fn write_ast_dot(ast: &Ast, ast_name: &str, seconds_since: u64) {
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
pub fn check_ast_equal(ast: &Ast, expected_ast: &Ast) {
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

/// Adds the tree to the AST for a function declaration with two arguments
pub fn add_basic_function_declaration(
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

/// adds a two param function as the child of the given parent
pub fn add_two_param_function(
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

/// adds a one param function as the child of the given parent
pub fn add_one_param_function(
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

/// Creates the expected tree a simple three argument function.
/// Factored out b/c we have a trailing comma and non-trailing comma test.
pub fn add_function_call_multiple_arguments(
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

/// Creates the expected tree a single terminal argument (e.g. symbol or number, not an expression) function.
pub fn add_function_call_one_arg(
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

pub fn add_function_call_no_arg(
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

/// Constructs expected ast for binary op and assignment composition
pub fn binary_op_and_assign_expected_ast(
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

/// Adds a for loop to the ast that matches the following syntax
/// for (a = 0; a < 10; a = a + 1;)
pub fn add_for_loop_declaration(
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
pub fn add_basic_for_loop(
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
