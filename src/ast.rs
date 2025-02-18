use std::fmt::{Write, Debug, Display};

use crate::lexer::{ TokenWithLocation, Token };

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Expression(Expression),
    Let {
        ident: String,
        expression: Expression
    },
    Assign {
        assignable: Assignable,
        expression: Expression
    },
    If {
        cond: Expression,
        body: Vec<Self>,
        r#else: Option<Vec<Self>>
    },
    While {
        cond: Expression,
        body: Vec<Self>
    },
    Return(Option<Expression>),
    FuncDecl(Function)
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub ident: String,
    pub params: Vec<String>,
    pub body: Vec<Statement>
}

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}({}) {{{}}}", &self.ident, join(&self.params, ", "), join(&self.body, " "))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Prefix {
        op: PrefixOp,
        expression: Box<Self>,
    },
    Infix {
        op: InfixOp,
        left: Box<Self>,
        right: Box<Self>
    },
    Group(Box<Self>),
    FuncCall {
        func: Assignable,
        params: Vec<Self>
    },
    Assignable(Assignable),
    Literal(Literal)
}

#[derive(Debug, Clone, PartialEq)]
pub enum Assignable {
    Var(String),
    ArrayIndex {
        array: Box<Self>,
        index: Box<Expression>
    },
    ObjectProp {
        obj: Box<Self>,
        prop: String
    },
}


#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    ArrayLit(Vec<Expression>),
    ObjectLit(Vec<(String, Expression)>),
    StringLit(String),
    NumberLit(i64),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum PrefixOp {
    Neg,
    Bang
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum InfixOp {
    Add,
    Sub,
    Mult,
    Div,
    Lt,
    Gt,
    Eq,
    NotEq
}

#[derive(Debug, PartialEq)]
pub struct TokenNotInfixOpErr(TokenWithLocation);

impl TryFrom<TokenWithLocation> for InfixOp {
    type Error = TokenNotInfixOpErr;

    fn try_from(value: TokenWithLocation) -> Result<Self, Self::Error> {
        match value.clone().to_token() {
            Token::PLUS => Ok(Self::Add),
            Token::MINUS => Ok(Self::Sub),
            Token::ASTERISK => Ok(Self::Mult),
            Token::SLASH => Ok(Self::Div),
            Token::LT => Ok(Self::Lt),
            Token::GT => Ok(Self::Gt),
            Token::EQ => Ok(Self::Eq),
            Token::NOTEQ => Ok(Self::NotEq),
            _ => Err(TokenNotInfixOpErr(value))
        }
    }
}

impl Display for TokenNotInfixOpErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Could not translate token {} to Infix Operand", self.0.ref_token())
    }
}

impl Display for PrefixOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Neg => write!(f, "-"),
            Self::Bang => write!(f, "!")
        }
    }
}

impl Display for InfixOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Add => f.write_char('+'),
            Self::Sub => f.write_char('-'),
            Self::Mult => f.write_char('*'),
            Self::Div => f.write_char('/'),
            Self::Lt => f.write_char('<'),
            Self::Gt => f.write_char('>'),
            Self::Eq => f.write_str("=="),
            Self::NotEq => f.write_str("!=")
        }
    }
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::NumberLit(num) => write!(f, "{num}"),
            Self::ArrayLit(arr) => {
                write!(f, "[{}]", join(arr, ", "))
            },
            Self::StringLit(string) => f.write_str(string),
            Self::ObjectLit(obj) => {
                let obj = obj.iter().map(|(ident, value)| {
                    let mut output = ident.clone();
                    output.push_str(": ");
                    output.push_str(&value.to_string());
                    output
                }).collect::<Vec<String>>().join(", ");
                write!(f, "{{{obj}}}")
            }
        }
    }
}

impl Display for Assignable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Var(var) => write!(f, "{var}"),
            Self::ArrayIndex { array, index } => write!(f, "{array}[{index}]"),
            Self::ObjectProp { obj, prop } => write!(f, "{obj}.{prop}")
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Prefix { op, expression } => write!(f, "{op}{expression}"),
            Self::Infix { op, left, right } => write!(f, "({left} {op} {right})"),
            Self::Group(exp) => write!(f, "({exp})"),
            Self::FuncCall { func, params } => {
                write!(f, "{func}({})", join(params, ", "))
            },
            Self::Assignable(assignable) => write!(f, "{assignable}"),
            Self::Literal(lit) => write!(f, "{lit}")
        }
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Expression(e) => write!(f, "{e}"),
            Self::Let { ident, expression } => write!(f, "let {ident} = {expression};"),
            Self::Assign { assignable, expression } => write!(f, "{assignable} = {expression};"),
            Self::Return(value) => {
                match value {
                    Some(value) => write!(f, "return {value};"),
                    None => write!(f, "return;"),
                }
            },
            Self::If { cond, body, r#else } => {
                let body = join(body, " ");
                write!(f, "if {cond} {{{body}}}")?;
                if let Some(block) = r#else {
                    write!(f, " else {}", join(block, " "))?;
                }
                Ok(())
            },
            Self::While { cond, body } => write!(f, "while {cond} {{{}}}", join(body, " ")),
            Self::FuncDecl(func) => {
                write!(f, "{func}")
            }
        }
    }
}

// Lowkey ripped from AntonioSarosi MKDB
pub fn join<'t, T: std::fmt::Display + 't>(vals: impl IntoIterator<Item = &'t T>, separator: &str) -> String {
    // Init output string
    let mut output = String::new();
    let mut vals = vals.into_iter();
    // Print first val in iterator without seperator
    if let Some(val) =  vals.next() {
        write!(output, "{}", &val).unwrap();
    };
    // all other values are prefixed with the seperator
    for val in vals {
        write!(output, "{separator}{val}").unwrap();
    }
    output
}
