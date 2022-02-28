use std::fmt::Display;

use crate::{
    cast,
    lexer::{default_range, OperatorKind, Range, Token, TokenKind},
};

// pub struct VariableDecleration {
//     identifier: String,
//     variable_type: Option<Box<ParseNode>>,
//     initializer: Option<Box<ParseNode>>,
// }

// #[derive(Debug)]
// pub struct BinaryExpression {
//     left: Box<ParseNode>,
//     operator: Operator,
//     right: Box<ParseNode>,
// }
#[derive(Debug, Clone, PartialEq)]
pub enum Loop {
    Infinite(/* Body */ Box<ParseNode>),
    Until(
        /* Condition */ Box<Expression>,
        /* Body */ Box<ParseNode>,
    ),
}

#[derive(Debug, Clone, PartialEq)]
pub struct LoopStatement {
    pub keyword: Range,
    pub loop_type: Loop,
    pub range: Range,
}

impl Loop {
    pub fn get_range(&self) -> Range {
        match self {
            Loop::Infinite(b) => b.get_range(),
            Loop::Until(e, b) => (e.get_range().0, b.get_range().1),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryExpression {
    pub left: Box<Expression>,
    pub operator: OperatorKind,
    pub right: Box<Expression>,
    pub range: Range,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnaryExpression {
    pub expression: Box<Expression>,
    pub operator: OperatorKind,
    pub range: Range,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfStatement {
    pub if_token: Range,
    pub condition: Box<Expression>,
    pub body: Box<ParseNode>,
    pub else_clause: Option<(
        /* else token */ Range,
        /* else clause body*/ Box<ParseNode>,
    )>,
    pub range: Range,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionCall {
    pub expression_to_call: Box<Expression>,
    pub arguments: Vec<Expression>,
    pub paren_tokens: Range,
    pub range: Range,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExpressionIndex {
    pub index_expression: Box<Expression>,
    pub index_value: Box<Expression>,
    pub square_range: Range,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Identifier(Token),
    Literal(Literal),
    BinaryExpression(BinaryExpression),
    UnaryExpression(UnaryExpression),
    FunctionCall(FunctionCall),
    Lambda(FunctionSignature, Box<ParseNode>),
    Index(ExpressionIndex),
}

impl Expression {
    pub fn get_range(&self) -> Range {
        match self {
            Expression::Identifier(t) => t.range,
            Expression::Literal(t) => t.get_range(),
            Expression::BinaryExpression(t) => t.range,
            Expression::UnaryExpression(t) => t.range,
            Expression::FunctionCall(t) => t.range,
            Expression::Lambda(t, b) => t.range,
            Expression::Index(t) => (t.index_expression.get_range().0, t.square_range.1),
        }
    }
}

// #[derive(Debug, Clone, PartialEq)]
// pub struct Expression {
//     kind: ExpressionKind,
//     range: Range,
// }

#[derive(Debug, Clone, PartialEq)]
struct IdentSymbol {
    pub identifier: Token,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeSymbol {
    pub symbol_type: Type,
    pub symbol: Token,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionSignature {
    pub parameters: Vec<TypeSymbol>,
    pub return_type: Box<Type>,
    pub parens: Range,
    pub range: Range,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ArrayType {
    pub base_type: Box<Type>,
    pub size: Option<(/* Colon */ Range, /* Size */ usize)>,
    pub range: Range,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ReferenceType {
    pub reference: Range,
    pub base_type: Box<Type>,
    pub range: Range,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    NamedType(Token),
    Int(u8, Range),
    Uint(u8, Range),
    Bool(Range),
    Float(Range),
    Char(Range),
    ArrayType(ArrayType),
    FunctionType(FunctionSignature),
    ReferenceType(ReferenceType),
}

impl Type {
    pub fn get_range(&self) -> Range {
        match self {
            Type::NamedType(t) => t.range,
            Type::Int(_, t) => t.clone(),
            Type::Uint(_, t) => t.clone(),
            Type::Bool(t) => t.clone(),
            Type::Float(t) => t.clone(),
            Type::Char(t) => t.clone(),
            Type::ArrayType(t) => t.range,
            Type::FunctionType(t) => t.range,
            Type::ReferenceType(t) => t.range,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Punctuation {
    Comma,
    FunctionArrow,
}

#[derive(Debug, Clone, PartialEq)]
pub struct VariableDecleration {
    pub variable_type: Type,
    pub identifier: Token,
    pub possible_initializer: Option<(
        /* Initializer */ Box<Expression>,
        /* Position of assignment operator*/ Range,
    )>,
    pub range: Range,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDecleration {
    pub identifier: Token,
    pub function_type: FunctionSignature,
    pub body: Box<ParseNode>,
    pub range: Range,
}

impl FunctionDecleration {
    fn write(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        index: u32,
        indent: &String,
        last: bool,
    ) -> std::fmt::Result {
        let name = cast!(&self.identifier.token_type, TokenKind::Ident);
        // let nindent = format!(
        //     "{}{}",
        //     indent,
        //     if index == 0 {
        //         ""
        //     } else if last {
        //         "    "
        //     } else {
        //         "│   "
        //     }
        // );
        writeln!(f, "Function: {}", name)?;
        self.body.write(f, index + 1, indent, last)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructDecleration {
    pub struct_keyword: Range,
    pub token: Token,
    pub fields: Vec<TypeSymbol>,
    pub range: Range,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeDecleration {
    pub type_keyword: Range,
    pub token: Token,
    pub old_type: Type,
    pub assignment: Range,
    pub range: Range,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ImportDecleration {
    pub import_keyword: Range,
    pub path: Vec<Expression>,
    pub range: Range,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParseNode {
    None,
    Expression(Expression, Range),
    Type(Type, Range),
    VariableDecleration(VariableDecleration),
    FunctionDecleration(FunctionDecleration),
    Block(/* Statements */ Vec<ParseNode>, Range),
    Return(Box<Expression>, Range),
    StructDecleration(StructDecleration),
    TypeDecleration(TypeDecleration),
    IfStatement(IfStatement),
    LoopStatement(LoopStatement),
    // Tag(/* Expression of tag */ Expression, Range),
    // TagCollection(Vec<ParseNode>, Option<Box<ParseNode>>, Range),
    Import(ImportDecleration),
    Punctuation(Punctuation, Range),
}

impl ParseNode {
    pub fn get_range(&self) -> Range {
        match self {
            ParseNode::Expression(_, r) => r.clone(),
            ParseNode::Type(_, r) => r.clone(),
            ParseNode::VariableDecleration(v) => v.range,
            ParseNode::FunctionDecleration(f) => f.range,
            ParseNode::Block(_, f) => f.clone(),
            ParseNode::Return(_, f) => f.clone(),
            ParseNode::StructDecleration(s) => s.range,
            ParseNode::TypeDecleration(t) => t.range,
            ParseNode::IfStatement(i) => i.range,
            ParseNode::LoopStatement(l) => l.range,
            ParseNode::Import(i) => i.range,
            ParseNode::Punctuation(_, p) => p.clone(),
            ParseNode::None => default_range(),
        }
    }
}

impl Display for ParseNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.write(f, 0, &"".to_string(), false)
    }
}

impl ParseNode {
    fn write(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        index: u32,
        indent: &String,
        last: bool,
    ) -> std::fmt::Result {
        write!(f, "{}", indent)?;
        if index != 0 {
            write!(f, "{}", if last { "└──" } else { "├──" })?;
        }
        let nindent = format!(
            "{}{}",
            indent,
            if index == 0 {
                ""
            } else if last {
                "    "
            } else {
                "│   "
            }
        );
        let res = match self {
            ParseNode::Expression(e, _) => write!(f, "{:?}", e),
            ParseNode::Type(e, _) => write!(f, "{:?}", e),
            ParseNode::VariableDecleration(e) => write!(f, "{:?}", e),
            ParseNode::FunctionDecleration(e) => e.write(f, index+1, &nindent, true),
            ParseNode::Block(e, _) => {
                write!(f, "Block\n")?;
                e.iter().enumerate().for_each(|(i, v)| {
                    ParseNode::write(v, f, index + 1, &nindent, i == e.len() - 1).unwrap();
                });
                Ok(())
            }
            ParseNode::Return(e, _) => write!(f, "{:?}", e),
            ParseNode::StructDecleration(e) => write!(f, "{:?}", e),
            ParseNode::TypeDecleration(e) => write!(f, "{:?}", e),
            ParseNode::IfStatement(e) => write!(f, "{:?}", e),
            ParseNode::LoopStatement(e) => write!(f, "{:?}", e),
            ParseNode::Import(e) => write!(f, "{:?}", e),
            ParseNode::Punctuation(e, _) => write!(f, "{:?}", e),
            ParseNode::None => write!(f, "None"),
        };
        res
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ArrayInitializer {
    pub elements: Vec<Expression>,
    pub range: Range,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructInitializer {
    pub initializer_values: Vec<(String, Option<Expression>)>,
    pub range: Range,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Empty,
    Integer(u64, u8, Range),
    Float(f64, Range),
    Boolean(bool, Range),

    // The following varients are formed in the parser
    Array(ArrayInitializer),
    StructInitializer(StructInitializer),
    String(String, Range),
}

impl Literal {
    pub fn get_range(&self) -> Range {
        match self {
            Literal::Integer(_, _, r) => r.clone(),
            Literal::Float(_, r) => r.clone(),
            Literal::Boolean(_, r) => r.clone(),
            Literal::Array(r) => r.range.clone(),
            Literal::StructInitializer(r) => r.range.clone(),
            Literal::String(_, r) => r.clone(),
            Literal::Empty => default_range(),
        }
    }
}
