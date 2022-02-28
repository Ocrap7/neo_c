use std::{
    collections::{linked_list::Cursor, LinkedList},
    fmt::{self, Display},
};

use crate::{
    ast::{
        ArrayInitializer, ArrayType, BinaryExpression, Expression, ExpressionIndex, FunctionCall,
        FunctionDecleration, FunctionSignature, ImportDecleration, Literal, ParseNode,
        ReferenceType, StructDecleration, StructInitializer, Type, TypeDecleration, TypeSymbol,
        UnaryExpression, VariableDecleration,
    },
    lexer::{default_range, Keyword, KeywordKind, Operator, OperatorKind, Token, TokenKind},
};

#[derive(Debug)]
pub struct ParseError {
    error: String,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.error)
    }
}

impl ParseError {
    fn new(error: &String) -> Self {
        ParseError {
            error: error.clone(),
        }
    }
}

// #[derive(Debug)]
// pub struct ParseNode {
//     pub children: Vec<ParseNode>,
//     pub entry: GrammarItem,
// }

pub fn parse_from_tokens(tokens: &LinkedList<&Token>) -> Result<ParseNode, ParseError> {
    let mut it = tokens.cursor_front();
    let mut statements = vec![];

    let start = if let Some(t) = it.current() {
        t.range
    } else {
        default_range()
    };

    while let Some(_) = it.current() {
        let statement = parse_top_level_statement(&mut it)?;
        statements.push(statement);
    }

    let end = if let Some(t) = it.current() {
        t.range
    } else if let Some(t) = it.back() {
        t.range
    } else {
        default_range()
    };
    Ok(ParseNode::Block(statements, (start.0, end.1)))
}

fn parse_top_level_statement(tokens: &mut Cursor<&Token>) -> Result<ParseNode, ParseError> {
    match tokens.current() {
        Some(t) => match t.token_type {
            TokenKind::Keyword(k) => match k.keyword {
                KeywordKind::Import => {
                    let res = parse_import(tokens);
                    expect(tokens, TokenKind::Semi)?;
                    res
                }
                KeywordKind::Type => {
                    tokens.move_next();
                    let new_type = expect(tokens, TokenKind::Ident(String::from("")))?;
                    let eq = expect(tokens, Operator::create_expect(OperatorKind::Assignment))?;
                    let current_type = parse_type(tokens)?;

                    expect(tokens, TokenKind::Semi)?;

                    let end = current_type.get_range().1;

                    let td = TypeDecleration {
                        type_keyword: t.range,
                        token: new_type.clone(),
                        old_type: current_type,
                        assignment: eq.range,
                        range: (t.range.0, end),
                    };

                    Ok(ParseNode::TypeDecleration(td))
                }
                KeywordKind::Bool
                | KeywordKind::Int
                | KeywordKind::Uint
                | KeywordKind::Char
                | KeywordKind::Float => Ok(parse_var_or_func(tokens)?),
                KeywordKind::Struct => parse_struct(tokens),
                _ => Err(ParseError::new(&format!("Unexpected keyword {:?}", t))),
            },
            TokenKind::Ident(_) => Ok(parse_var_or_func(tokens)?),
            _ => Err(ParseError::new(&format!("Unexpected token {:?}", t))),
        },
        None => Ok(ParseNode::None),
    }
}

fn parse_statement(tokens: &mut Cursor<&Token>) -> Result<ParseNode, ParseError> {
    match tokens.current() {
        Some(t) => match t.token_type {
            TokenKind::Ident(_) => Ok(parse_var_or_func(tokens)?),
            TokenKind::Keyword(k) => match k.keyword {
                KeywordKind::Bool
                | KeywordKind::Int
                | KeywordKind::Uint
                | KeywordKind::Char
                | KeywordKind::Float => Ok(parse_var_or_func(tokens)?),
                _ => {
                    let expr = parse_expression(tokens, 0)?;
                    expect(tokens, TokenKind::Semi)?;
                    let rng = expr.get_range();
                    Ok(ParseNode::Expression(expr, rng))
                }
            },
            TokenKind::OpenBrace => parse_block_statement(tokens),
            _ => {
                let expr = parse_expression(tokens, 0)?;
                expect(tokens, TokenKind::Semi)?;
                let rng = expr.get_range();
                Ok(ParseNode::Expression(expr, rng))
            }
        },
        None => Ok(ParseNode::None),
    }
}

fn parse_struct(tokens: &mut Cursor<&Token>) -> Result<ParseNode, ParseError> {
    let kw = expect(tokens, Keyword::create_expect(KeywordKind::Struct))?;
    let identifier = expect(tokens, TokenKind::Ident("".to_string()))?;

    let ob = expect(tokens, TokenKind::OpenBrace)?;
    let mut fields = vec![];

    while let Some(_) = tokens.current() {
        if let Some(Token {
            token_type: TokenKind::CloseBrace,
            ..
        }) = tokens.current()
        {
            break;
        }
        let field_type = parse_type(tokens)?;

        let identifier = expect(tokens, TokenKind::Ident("".to_string()))?;
        let ts = TypeSymbol {
            symbol_type: field_type,
            symbol: identifier.clone(),
        };
        expect(tokens, TokenKind::Semi)?;
        fields.push(ts);
    }

    let cb = expect(tokens, TokenKind::CloseBrace)?;
    let sd = StructDecleration {
        struct_keyword: kw.clone().range,
        token: identifier.clone(),
        fields,
        range: (kw.range.0, cb.range.1),
    };
    Ok(ParseNode::StructDecleration(sd))
}

fn parse_function_call(
    tokens: &mut Cursor<&Token>,
    to_be_called: Expression,
) -> Result<Expression, ParseError> {
    let op = expect(tokens, TokenKind::OpenParen)?;
    let mut args = vec![];
    while let Some(_) = tokens.current() {
        if let Some(Token {
            token_type: TokenKind::CloseParen,
            ..
        }) = tokens.current()
        {
            break;
        }
        args.push(parse_expression(tokens, 0)?);
        match tokens.current() {
            Some(t) => match t.token_type {
                TokenKind::Comma => tokens.move_next(),
                TokenKind::CloseParen => {
                    break;
                }
                _ => {
                    return Err(ParseError::new(&format!(
                        "Expected comma or closing parenthesis!"
                    )))
                }
            },
            None => return Err(ParseError::new(&format!("Expected token!"))),
        };
    }
    let cp = expect(tokens, TokenKind::CloseParen)?;
    let start = to_be_called.get_range().0;
    let fc = FunctionCall {
        expression_to_call: Box::new(to_be_called),
        arguments: args,
        paren_tokens: (op.range.0, cp.range.1),
        range: (start, cp.range.1),
    };
    Ok(Expression::FunctionCall(fc))
}

fn parse_var_or_func(tokens: &mut Cursor<&Token>) -> Result<ParseNode, ParseError> {
    let ttype = parse_type(tokens)?;
    let ident_token = expect(tokens, TokenKind::Ident("".to_string()))?;

    if let Some(t) = tokens.current() {
        match t.token_type {
            TokenKind::OpenParen => {
                return parse_function(tokens, ttype, ident_token);
            }
            _ => return parse_variable_decleration(tokens, ttype, ident_token),
        }
    }
    Err(ParseError::new(&String::from(
        "Could not parse funciton or variable",
    )))
}

fn parse_function(
    tokens: &mut Cursor<&Token>,
    ret_type: Type,
    ident_token: &Token,
) -> Result<ParseNode, ParseError> {
    // let ret_type = parse_type(tokens)?;
    let start = ret_type.get_range().0;

    // let ident_token = expect(tokens, TokenKind::Ident("".to_string()))?;
    let fn_type = parse_function_type(tokens, ret_type)?;
    let body = parse_statement(tokens)?;

    let end = body.get_range().1;

    let fd = FunctionDecleration {
        identifier: ident_token.clone(),
        function_type: fn_type,
        body: Box::new(body),
        range: (start, end),
    };

    Ok(ParseNode::FunctionDecleration(fd))
}

fn parse_function_type(
    tokens: &mut Cursor<&Token>,
    return_type: Type,
) -> Result<FunctionSignature, ParseError> {
    let op = expect(tokens, TokenKind::OpenParen)?;
    let mut params = vec![];
    while let Some(_) = tokens.current() {
        if let Some(Token {
            token_type: TokenKind::CloseParen,
            ..
        }) = tokens.current()
        {
            break;
        }
        let parameter_type = parse_type(tokens)?;
        let identifier = expect(tokens, TokenKind::Ident("".to_string()))?;
        let ts = TypeSymbol {
            symbol_type: parameter_type,
            symbol: identifier.clone(),
        };
        params.push(ts);

        match tokens.current() {
            Some(t) => match t.token_type {
                TokenKind::Comma => tokens.move_next(),
                TokenKind::CloseParen => {
                    break;
                }
                _ => {
                    return Err(ParseError::new(&format!(
                        "Expected comma or closing parenthesis!"
                    )))
                }
            },
            None => return Err(ParseError::new(&format!("Expected token!"))),
        };
    }

    let cp = expect(tokens, TokenKind::CloseParen)?;

    let start = return_type.get_range().0;
    Ok(FunctionSignature {
        parameters: params,
        return_type: Box::new(return_type),
        parens: (op.range.0, cp.range.1),
        range: (start, cp.range.1),
    })
}

fn parse_block_statement(tokens: &mut Cursor<&Token>) -> Result<ParseNode, ParseError> {
    let op = expect(tokens, TokenKind::OpenBrace)?;
    let mut statements = vec![];
    while let Some(_) = tokens.current() {
        if let Some(Token {
            token_type: TokenKind::CloseBrace,
            ..
        }) = tokens.current()
        {
            break;
        }

        statements.push(parse_statement(tokens)?);

        match tokens.current() {
            Some(t) => match t.token_type {
                TokenKind::CloseBrace => {
                    break;
                }
                _ => (),
            },
            None => return Err(ParseError::new(&format!("Expected token!"))),
        };
    }
    let cp = expect(tokens, TokenKind::CloseBrace)?;
    Ok(ParseNode::Block(statements, (op.range.0, cp.range.1)))
}

fn parse_expression(tokens: &mut Cursor<&Token>, prev_prec: u8) -> Result<Expression, ParseError> {
    let mut left = if let Some(Token {
        token_type: TokenKind::Operator(o),
        ..
    }) = tokens.current()
    {
        let uprec = unary_precedence(*o);
        if uprec != 0 && uprec >= prev_prec {
            tokens.move_next();
            let right = parse_expression(tokens, uprec);
            match right {
                Ok(n) => {
                    let end = n.get_range().1;
                    let ue = UnaryExpression {
                        expression: Box::new(n),
                        operator: o.operator,
                        range: (o.range.0, end),
                    };
                    Ok(Expression::UnaryExpression(ue))
                }
                Err(_) => right,
            }
        } else {
            parse_expression(tokens, 0)
        }
    } else {
        parse_primary(tokens)
    };

    while let Some(Token {
        token_type: TokenKind::Operator(o),
        ..
    }) = tokens.current()
    {
        let prec = binary_precedence(*o);
        if prec <= prev_prec || prec == 0 {
            break;
        }
        tokens.move_next();

        let lleft = left?;
        let right = parse_expression(tokens, prec)?;
        let start = lleft.get_range().0;
        let end = right.get_range().1;
        let be = BinaryExpression {
            left: Box::new(lleft),
            operator: o.operator,
            right: Box::new(right),
            range: (start, end),
        };
        left = Ok(Expression::BinaryExpression(be));
    }

    let nleft = left?;
    let nnleft = nleft.clone();

    while let Some(Token { token_type, .. }) = tokens.current() {
        let prec = postfix_precedence(token_type);
        if prec <= prev_prec || prec == 0 {
            break;
        }
        match token_type {
            TokenKind::OpenParen => return parse_function_call(tokens, nnleft),
            TokenKind::OpenBracket => {
                let ob = expect(tokens, TokenKind::OpenBracket)?;
                let value = parse_expression(tokens, 0)?;
                let cb = expect(tokens, TokenKind::CloseBracket)?;
                let idx = ExpressionIndex {
                    index_expression: Box::new(nnleft),
                    index_value: Box::new(value),
                    square_range: (ob.range.0, cb.range.1),
                };

                return Ok(Expression::Index(idx));
            }
            _ => return Ok(nnleft),
        }
    }

    Ok(nnleft)
}

fn parse_variable_decleration(
    tokens: &mut Cursor<&Token>,
    var_type: Type,
    identifier: &Token,
) -> Result<ParseNode, ParseError> {
    let var_initializer = match tokens.current() {
        Some(Token {
            token_type:
                TokenKind::Operator(Operator {
                    operator: OperatorKind::Assignment,
                    ..
                }),
            ..
        }) => {
            let tok = tokens.current().unwrap();
            tokens.move_next();
            Some((Box::new(parse_expression(tokens, 0)?), tok.range))
        }
        Some(Token {
            token_type: TokenKind::OpenBrace,
            ..
        }) => {
            let init = parse_struct_initializer(tokens)?;
            let range = init.get_range();
            Some((Box::new(init), range))
        }
        _ => None,
    };

    expect(tokens, TokenKind::Semi)?;

    let end = match &var_initializer {
        Some(s) => s.1,
        None => identifier.range,
    };
    let start = var_type.get_range().0;
    let vd = VariableDecleration {
        variable_type: var_type,
        possible_initializer: var_initializer,
        identifier: identifier.clone(),
        range: (start, end.1),
    };
    Ok(ParseNode::VariableDecleration(vd))
}

fn parse_import(tokens: &mut Cursor<&Token>) -> Result<ParseNode, ParseError> {
    let keyword = expect(tokens, Keyword::create_expect(KeywordKind::Import))?;
    let mut modules = vec![];
    let thing = parse_expression(tokens, 0)?;
    fn add_wild(modules: &mut Vec<Expression>, node: &Expression) {
        match node {
            Expression::BinaryExpression(BinaryExpression { left, right, .. }) => {
                add_wild(modules, left.as_ref());
                add_wild(modules, right.as_ref());
            }
            Expression::Identifier(_) => {
                modules.push(node.clone());
            }
            _ => (),
        }
    }
    add_wild(&mut modules, &thing);
    let end = match modules.last() {
        Some(m) => m.get_range().1,
        None => keyword.range.1,
    };
    let id = ImportDecleration {
        import_keyword: keyword.range,
        path: modules,
        range: (keyword.range.0, end),
    };

    Ok(ParseNode::Import(id))
}

fn parse_primary(tokens: &mut Cursor<&Token>) -> Result<Expression, ParseError> {
    match tokens.current() {
        Some(t) => match t {
            Token {
                token_type: TokenKind::OpenParen,
                ..
            } => {
                tokens.move_next();
                let expr = parse_expression(tokens, 0)?;
                expect(tokens, TokenKind::CloseParen)?;
                Ok(expr)
            }
            _ => parse_literal(tokens),
        },
        None => parse_literal(tokens),
    }
}

fn parse_literal(tokens: &mut Cursor<&Token>) -> Result<Expression, ParseError> {
    match tokens.current() {
        Some(t) => match t {
            Token {
                token_type: TokenKind::Literal(a),
                ..
            } => {
                tokens.move_next();
                Ok(Expression::Literal(a.clone()))
            }
            Token {
                token_type: TokenKind::OpenBracket,
                ..
            } => parse_array_literal(tokens),
            Token {
                token_type: TokenKind::OpenBrace,
                ..
            } => parse_struct_initializer(tokens),
            Token {
                token_type: TokenKind::Ident(_),
                ..
            } => parse_ident(tokens),
            Token {
                token_type: TokenKind::Keyword(k),
                ..
            } => match k.keyword {
                KeywordKind::True => {
                    tokens.move_next();
                    Ok(Expression::Literal(Literal::Boolean(true, t.range)))
                }
                KeywordKind::False => {
                    tokens.move_next();
                    Ok(Expression::Literal(Literal::Boolean(false, t.range)))
                }
                _ => Err(ParseError::new(&format!(
                    "Keyword {:?} is not a valid literal!",
                    k
                ))),
            },
            _ => Err(ParseError::new(&"Unkown literal value!".to_string())),
        },
        None => Err(ParseError::new(&"Unkown literal value!".to_string())),
    }
}

fn parse_ident(tokens: &mut Cursor<&Token>) -> Result<Expression, ParseError> {
    let possible_type = parse_type(tokens)?;

    match possible_type {
        Type::NamedType(t) => match t.token_type {
            TokenKind::Ident(_) => Ok(Expression::Identifier(t)),
            _ => Err(ParseError::new(&format!("Unexpected type in expression!"))),
        },
        _ => Err(ParseError::new(&format!("Unexpected type in expression!"))),
    }
}

fn parse_struct_initializer(tokens: &mut Cursor<&Token>) -> Result<Expression, ParseError> {
    let ob = expect(tokens, TokenKind::OpenBrace)?;
    let mut key_values = vec![];

    while let Some(_) = tokens.current() {
        if let Some(Token {
            token_type: TokenKind::CloseBrace,
            ..
        }) = tokens.current()
        {
            break;
        }
        let key = expect(tokens, TokenKind::Ident("".to_string()))?;
        let key_string = match &key.token_type {
            TokenKind::Ident(s) => s.clone(),
            _ => panic!("Shouldn't be here!"),
        };
        let value = if let Some(Token {
            token_type: TokenKind::Colon,
            ..
        }) = tokens.current()
        {
            tokens.move_next();
            Some(parse_expression(tokens, 0)?)
        } else {
            None
        };
        key_values.push((key_string, value));

        match tokens.current() {
            Some(t) => match t.token_type {
                TokenKind::Comma => tokens.move_next(),
                TokenKind::CloseBrace => {
                    break;
                }
                _ => {
                    return Err(ParseError::new(&format!(
                        "Expected comma or closing brace!"
                    )))
                }
            },
            None => return Err(ParseError::new(&format!("Expected token!"))),
        };
    }

    let cb = expect(tokens, TokenKind::CloseBrace)?;

    let si = StructInitializer {
        initializer_values: key_values,
        range: (ob.range.0, ob.range.1),
    };
    Ok(Expression::Literal(Literal::StructInitializer(si)))
}

fn parse_array_literal(tokens: &mut Cursor<&Token>) -> Result<Expression, ParseError> {
    let ob = expect(tokens, TokenKind::OpenBracket)?;
    let mut values = vec![];
    while let Some(_) = tokens.current() {
        if let Some(Token {
            token_type: TokenKind::CloseBracket,
            ..
        }) = tokens.current()
        {
            break;
        }
        let value = parse_expression(tokens, 0)?;
        values.push(value);
        match tokens.current() {
            Some(t) => match t.token_type {
                TokenKind::Comma => tokens.move_next(),
                TokenKind::CloseBracket => {
                    break;
                }
                _ => {
                    return Err(ParseError::new(&format!(
                        "Expected comma or closing bracket!"
                    )))
                }
            },
            None => return Err(ParseError::new(&format!("Expected token!"))),
        };
    }
    let cb = expect(tokens, TokenKind::CloseBracket)?;
    let ai = ArrayInitializer {
        elements: values,
        range: (ob.range.0, cb.range.1),
    };

    Ok(Expression::Literal(Literal::Array(ai)))
}

fn parse_type(tokens: &mut Cursor<&Token>) -> Result<Type, ParseError> {
    match tokens.current() {
        Some(t) => {
            let result = match t.token_type {
                TokenKind::Ident(_) => {
                    let token = (*t).clone();
                    tokens.move_next();
                    Ok(Type::NamedType(token))
                }
                TokenKind::Keyword(k) => {
                    tokens.move_next();
                    match k.keyword {
                        KeywordKind::Int => Ok(Type::Int(8, t.range)),
                        KeywordKind::Uint => Ok(Type::Uint(8, t.range)),
                        KeywordKind::Bool => Ok(Type::Bool(t.range)),
                        KeywordKind::Char => Ok(Type::Char(t.range)),
                        KeywordKind::Float => Ok(Type::Float(t.range)),
                        _ => Err(ParseError::new(&format!("{:?} is not a valid type!", k))),
                    }
                }
                _ => Err(ParseError::new(&format!("{:?} is not a valid type!", t))),
            };

            let mut result = result;
            while let Some(Token {
                token_type:
                    TokenKind::Operator(Operator {
                        operator: OperatorKind::BitAnd,
                        ..
                    }),
                ..
            }) = tokens.current()
            {
                let tok = tokens.current().unwrap();
                tokens.move_next();
                let rresult = result?;
                let start = rresult.get_range().0;
                let rt = ReferenceType {
                    base_type: Box::new(rresult),
                    reference: tok.range,
                    range: (start, tok.range.1),
                };
                result = Ok(Type::ReferenceType(rt));
            }

            if let Some(Token {
                token_type: TokenKind::OpenBracket,
                ..
            }) = tokens.current()
            {
                let ob = tokens.current().unwrap();
                tokens.move_next();
                let size = if let Some(Token {
                    token_type: TokenKind::Literal(Literal::Integer(_, _, _)),
                    ..
                }) = tokens.current()
                {
                    let size = tokens.current().unwrap();
                    tokens.move_next();

                    let numeric_size = match size {
                        Token {
                            token_type: TokenKind::Literal(Literal::Integer(i, _, _)),
                            ..
                        } => *i as usize,
                        _ => {
                            return Err(ParseError::new(&format!(
                                "Expected constant integer for array size!"
                            )));
                        }
                    };
                    Some(numeric_size)
                } else {
                    None
                };
                let cb = expect(tokens, TokenKind::CloseBracket)?;
                let size = size.map(|f| ((ob.range.0, cb.range.1), f));
                let result = result?;
                let start = result.get_range().0;
                let at = ArrayType {
                    base_type: Box::new(result),
                    size,
                    range: (start, cb.range.1),
                };
                return Ok(Type::ArrayType(at));
            }
            result
        }
        None => Err(ParseError::new(&format!("Expected more tokens for type!"))),
    }
}

fn unary_precedence(operator: Operator) -> u8 {
    match operator.operator {
        OperatorKind::Minus
        | OperatorKind::LogicalNot
        | OperatorKind::BitNot
        // | OperatorKind::DeRef
        | OperatorKind::BitAnd => 14,
        _ => 0,
    }
}

fn binary_precedence(operator: Operator) -> u8 {
    match operator.operator {
        OperatorKind::Assignment => 2,
        OperatorKind::LogicalOr => 3,
        OperatorKind::LogicalXor => 4,
        OperatorKind::LogicalAnd => 5,
        OperatorKind::BitOr => 6,
        OperatorKind::BitXor => 7,
        OperatorKind::BitAnd => 8,
        OperatorKind::Eq | OperatorKind::NEq => 9,
        OperatorKind::Lt | OperatorKind::LtEq | OperatorKind::Gt | OperatorKind::GtEq => 10,
        OperatorKind::BitLeft | OperatorKind::BitRight => 11,
        OperatorKind::Plus | OperatorKind::Minus | OperatorKind::Percent => 12,
        OperatorKind::Mult | OperatorKind::Divide => 13,
        OperatorKind::Dot => 15,
        _ => 0,
    }
}

fn postfix_precedence(token: &TokenKind) -> u8 {
    match token {
        TokenKind::OpenParen => 15,
        TokenKind::OpenBracket => 15,
        _ => 0,
    }
}

fn expect<'a>(
    tokens: &mut Cursor<&'a Token>,
    token_type: TokenKind,
) -> Result<&'a Token, ParseError> {
    match tokens.current() {
        Some(t) if std::mem::discriminant(&t.token_type) == std::mem::discriminant(&token_type) => {
            tokens.move_next();
            Ok(t)
        }
        Some(t) => {
            tokens.move_next();
            Err(ParseError::new(&format!(
                "Expected token {:?}, found token {:?}",
                token_type, t.token_type
            )))
        }
        None => {
            tokens.move_next();
            Err(ParseError::new(&format!(
                "Expected token {:?} ",
                token_type
            )))
        }
    }
}
