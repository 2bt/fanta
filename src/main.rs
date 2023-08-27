mod lexer;
use lexer::*;
use std::fmt::Write;

#[allow(dead_code)]
#[derive(Debug)]
enum OperationKind {
    // infix
    Or,
    And,
    BitOr,
    BitXor,
    BitAnd,
    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    LeftShift,
    RightShift,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    // prefix
    Not,
    Neg,
    Deref,
    Ref,
    // postfix
    Call,
    Index,
    Dot,
    Arrow,
}

impl OperationKind {
    fn binary_from_token_kind(t: TokenKind) -> Option<Self> {
        use OperationKind::*;
        Some(match t {
            TokenKind::Or => Or,
            TokenKind::And => And,
            TokenKind::BitOr => BitOr,
            TokenKind::BitXor => BitXor,
            TokenKind::BitAnd => BitAnd,
            TokenKind::Equal => Equal,
            TokenKind::NotEqual => NotEqual,
            TokenKind::Less => Less,
            TokenKind::LessEqual => LessEqual,
            TokenKind::Greater => Greater,
            TokenKind::GreaterEqual => GreaterEqual,
            TokenKind::LeftShift => LeftShift,
            TokenKind::RightShift => RightShift,
            TokenKind::Plus => Add,
            TokenKind::Minus => Sub,
            TokenKind::Asterisk => Mul,
            TokenKind::Slash => Div,
            TokenKind::Percent => Mod,
            _ => return None,
        })
    }
    fn precedence(&self) -> usize {
        use OperationKind::*;
        match self {
            Or => 1,
            And => 2,
            BitOr => 3,
            BitXor => 4,
            BitAnd => 5,
            Equal | NotEqual => 6,
            Less | LessEqual | Greater | GreaterEqual => 7,
            LeftShift | RightShift => 8,
            Add | Sub => 9,
            Mul | Div | Mod => 10,
            _ => 11,
        }
    }
}

// struct ExprIndex(usize);
// struct StmtIndex(usize);
// struct TypeIndex(usize);
// struct FuncIndex(usize);
// struct StructIndex(usize);
// struct GlobalVarIndex(usize);
// struct LocalVarIndex(usize);

#[allow(dead_code)]
#[derive(Debug)]
struct Operation {
    kind: OperationKind,
    x: usize,
    y: Option<usize>,
}

#[derive(Debug)]
enum Expression {
    Number(i32),
    Operation(Operation),
    FuncCall(usize, Vec<usize>),
    GlobalVar(usize),
    // LocalVar(usize),
}
enum Statement {
    Expression(usize),
    Assignment(usize, usize),
    Block(Vec<usize>),
    While(usize, usize),
    If(usize, usize, Option<usize>),
    Return(Option<usize>),
}

#[allow(dead_code)]
#[derive(PartialEq)]
enum DataType {
    Undefined,
    Void,
    Int,
    Pointer { datatype: usize },
    Array { len: usize, datatype: usize },
    Struct { strct: usize },
}

#[allow(dead_code)]
struct Member {
    name: String,
    datatype: usize,
}

#[allow(dead_code)]
#[derive(Default)]
struct Struct {
    name: String,
    members: Vec<Member>,
}

#[allow(dead_code)]
struct Function {
    name: String,
    datatype: usize,
    params: Vec<Member>,
    root: usize,
}

struct Variable {
    name: String,
    datatype: usize,
    offset: usize,
}

#[allow(dead_code)]
struct Parser<'a> {
    lexer: Lexer<'a>,
    token: Token,

    datatypes: Vec<DataType>,
    structs: Vec<Struct>,
    // constants
    globals: Vec<Variable>,

    functions: Vec<Function>,
    expressions: Vec<Expression>,
    statements: Vec<Statement>,
}

#[derive(Debug)]
enum ErrorKind {
    Token,
    Expect(TokenKind),
    Name,
}
#[derive(Debug)]
struct Error(Token, ErrorKind);

impl<'a> Parser<'a> {
    fn fmt_expr(&self, n: usize, f: &mut String) {
        use OperationKind::*;
        match &self.expressions[n] {
            Expression::Number(x) => write!(f, "{x}").unwrap(),
            Expression::Operation(o) => match o.kind {
                Neg => {
                    write!(f, "-").unwrap();
                    self.fmt_expr(o.x, f);
                }
                _ => {
                    write!(f, "(").unwrap();
                    self.fmt_expr(o.x, f);
                    write!(
                        f,
                        " {} ",
                        match o.kind {
                            Add => "+",
                            Sub => "-",
                            Mul => "*",
                            Div => "/",
                            Mod => "%",
                            _ => "???",
                        }
                    )
                    .unwrap();
                    self.fmt_expr(o.y.unwrap(), f);
                    write!(f, ")").unwrap();
                }
            },
            Expression::GlobalVar(n) => {
                write!(f, "{}", &self.globals[*n].name).unwrap();
            }
            e => write!(f, "{:?}", e).unwrap(),
        }
    }

    fn fmt_datatype(&self, dt: usize, f: &mut String) {
        match &self.datatypes[dt] {
            DataType::Undefined => write!(f, "undefined").unwrap(),
            DataType::Void => write!(f, "void").unwrap(),
            DataType::Int => write!(f, "int").unwrap(),
            DataType::Pointer { datatype } => {
                write!(f, "*").unwrap();
                self.fmt_datatype(*datatype, f);
            }
            DataType::Array { len, datatype } => {
                write!(f, "[{}]", len).unwrap();
                self.fmt_datatype(*datatype, f);
            }
            DataType::Struct { .. } => {
                write!(f, "<TODO>").unwrap();
            }
        }
    }

    fn expr_to_string(&self, n: usize) -> String {
        let mut s = String::new();
        self.fmt_expr(n, &mut s);
        s
    }
    fn datatype_to_string(&self, n: usize) -> String {
        let mut s = String::new();
        self.fmt_datatype(n, &mut s);
        s
    }

    fn new(code: &'a str) -> Self {
        Parser {
            lexer: Lexer::new(code),
            token: Token {
                kind: TokenKind::Invalid,
                pos: 0,
            },
            datatypes: Default::default(),
            structs: Default::default(),
            globals: Default::default(),
            functions: Default::default(),
            expressions: Default::default(),
            statements: Default::default(),
        }
    }

    fn next_token(&mut self) -> Token {
        let tok = self.token;
        self.token = self.lexer.next_token();
        tok
    }

    fn expect(&mut self, t: TokenKind) -> Result<Token, Error> {
        let tok = self.next_token();
        if tok.kind != t {
            return Err(Error(tok, ErrorKind::Expect(t)));
        }
        Ok(tok)
    }

    fn add_datatype(&mut self, d: DataType) -> usize {
        match self.datatypes.iter().position(|dt| *dt == d) {
            Some(n) => n,
            None => {
                let n = self.datatypes.len();
                self.datatypes.push(d);
                n
            }
        }
    }

    fn parse_datatype(&mut self) -> Result<usize, Error> {
        let tok = self.next_token();
        Ok(match tok.kind {
            TokenKind::Int => self.add_datatype(DataType::Int),
            TokenKind::Asterisk => {
                let datatype = self.parse_datatype()?;
                self.add_datatype(DataType::Pointer { datatype })
            }
            TokenKind::LeftBracket => {
                let tok = self.expect(TokenKind::Number)?; // TODO: support for constant
                let len = self
                    .lexer
                    .number_value(&tok)
                    .try_into()
                    .map_err(|_| Error(tok, ErrorKind::Token))?;
                self.expect(TokenKind::RightBracket)?;
                let datatype = self.parse_datatype()?;
                self.add_datatype(DataType::Array { len, datatype })
            }
            _ => return Err(Error(tok, ErrorKind::Token)),
        })
    }

    fn datatype_size(&self, i: usize) -> usize {
        match self.datatypes[i] {
            DataType::Undefined | DataType::Void => 0,
            DataType::Int | DataType::Pointer { .. } => 1,
            DataType::Array { len, datatype } => len * self.datatype_size(datatype),
            DataType::Struct { .. } => unimplemented!(),
        }
    }

    fn add_expr(&mut self, e: Expression) -> usize {
        let n = self.expressions.len();
        self.expressions.push(e);
        n
    }

    fn add_stmt(&mut self, s: Statement) -> usize {
        let n = self.statements.len();
        self.statements.push(s);
        n
    }

    fn parse_expr(&mut self, precedence: usize) -> Result<usize, Error> {
        use TokenKind::*;
        let tok = self.next_token();
        let mut n = match tok.kind {
            Number => self.add_expr(Expression::Number(self.lexer.number_value(&tok))),
            LeftParen => {
                let n = self.parse_expr(0)?;
                self.expect(RightParen)?;
                n
            }
            Minus => {
                let n = self.parse_expr(999)?;
                if let Expression::Number(x) = &mut self.expressions[n] {
                    *x = -*x;
                    n
                } else {
                    self.add_expr(Expression::Operation(Operation {
                        kind: OperationKind::Neg,
                        x: n,
                        y: None,
                    }))
                }
            }
            Ident => {
                let name = self.lexer.ident_name(&tok);
                if self.token.kind == TokenKind::LeftParen {
                    // TODO function call
                    self.next_token();
                    self.expect(TokenKind::RightParen)?;
                    self.add_expr(Expression::FuncCall(0, vec![]))
                } else {
                    // global variable
                    let Some(i) = self.globals.iter().position(|v| v.name == name) else {
                        println!("global var {name} not known");
                        return Err(Error(tok, ErrorKind::Name));
                    };
                    self.add_expr(Expression::GlobalVar(i))
                }
            }

            _ => return Err(Error(tok, ErrorKind::Token)),
        };

        // TODO: postfix

        // infix
        while let Some(op) = OperationKind::binary_from_token_kind(self.token.kind) {
            let prec = op.precedence();
            if prec <= precedence {
                break;
            }
            self.next_token();
            let m = self.parse_expr(prec)?;
            n = self.add_expr(Expression::Operation(Operation {
                x: n,
                y: Some(m),
                kind: op,
            }));
        }
        Ok(n)
    }

    fn parse_block_stmt(&mut self) -> Result<usize, Error> {
        self.expect(TokenKind::LeftBrace)?;
        let mut v = vec![];
        while self.token.kind != TokenKind::RightBrace {
            v.push(self.parse_simple_stmt()?);
        }
        self.next_token();
        Ok(self.add_stmt(Statement::Block(v)))
    }

    fn parse_simple_stmt(&mut self) -> Result<usize, Error> {
        let tok = self.token;
        Ok(match tok.kind {
            TokenKind::Var => {
                self.next_token();
                let ident = self.expect(TokenKind::Ident)?;
                let name = self.lexer.ident_name(&ident);
                if self.globals.iter().position(|v| v.name == name).is_some() {
                    return Err(Error(tok, ErrorKind::Name));
                }
                let offset = match self.globals.last() {
                    Some(v) => v.offset + self.datatype_size(v.datatype),
                    None => 0,
                };

                // type annotation
                let datatype = if self.token.kind == TokenKind::Colon {
                    self.next_token();
                    Some(self.parse_datatype()?)
                } else {
                    // TODO: make type annotation optional if there's an assignment
                    return Err(Error(tok, ErrorKind::Token));
                };

                let g = self.globals.len();
                self.globals.push(Variable {
                    name: name.to_owned(),
                    datatype: datatype.unwrap(),
                    offset,
                });

                if self.token.kind == TokenKind::Assign {
                    self.next_token();
                    let e = self.add_expr(Expression::GlobalVar(g));
                    let f = self.parse_expr(0)?;
                    self.expect(TokenKind::Semicolon)?;
                    self.add_stmt(Statement::Assignment(e, f))
                    // TODO: check that expr is lvalue
                } else {
                    self.expect(TokenKind::Semicolon)?;
                    // nothing really to do here
                    // just return an empty block 🤷
                    self.add_stmt(Statement::Block(vec![]))
                }
            }

            TokenKind::If => {
                self.next_token();
                let cond = self.parse_expr(0)?;
                let then = self.parse_block_stmt()?;
                let else_ = match self.token.kind {
                    TokenKind::Else => {
                        self.next_token();
                        Some(self.parse_block_stmt()?)
                    }
                    _ => None,
                };
                self.add_stmt(Statement::If(cond, then, else_))
            }

            TokenKind::While => {
                self.next_token();
                let cond = self.parse_expr(0)?;
                let body = self.parse_block_stmt()?;
                self.add_stmt(Statement::While(cond, body))
            }

            TokenKind::Return => {
                self.next_token();
                let val = match self.token.kind {
                    TokenKind::Semicolon => None,
                    _ => Some(self.parse_expr(0)?),
                };
                self.expect(TokenKind::Semicolon)?;
                self.add_stmt(Statement::Return(val))
            }

            _ => {
                // primary expression or assignment
                let e = self.parse_expr(999)?;
                let tok = self.next_token();
                match tok.kind {
                    TokenKind::Semicolon => {
                        self.next_token();
                        self.add_stmt(Statement::Expression(e))
                        // TODO: check that expr is a function call
                    }
                    TokenKind::Assign => {
                        let f = self.parse_expr(0)?;
                        self.expect(TokenKind::Semicolon)?;
                        self.add_stmt(Statement::Assignment(e, f))
                        // TODO: check that expr is lvalue
                    }
                    _ => return Err(Error(tok, ErrorKind::Token)),
                }
            }
        })
    }

    fn parse(&mut self) {
        self.next_token();

        let mut run = || -> Result<(), Error> {
            self.parse_block_stmt()?;

            // let e = self.parse_expr(0)?;
            // println!("{}", self.expr_to_string(e));

            Ok(())
        };
        if let Err(Error(tok, kind)) = run() {
            let code = self.lexer.code;
            let line = 1 + code.as_bytes()[..tok.pos]
                .iter()
                .filter(|c| **c == b'\n')
                .count();
            let start = code[..tok.pos].rfind('\n').map(|x| x + 1).unwrap_or(0);
            let end = tok.pos + code[tok.pos..].find('\n').unwrap_or(code.as_bytes().len());
            println!("Error ({kind:?},{tok:?}) at line {line}:");
            println!("{}", &code[start..end]);
            println!("{:>1$}", "^", tok.pos - start + 1);
        }
    }
}

const CODE: &str = "

# int
# **[4]*int
# 3 * (1 + 2) - (3) - (3 + 4) * -(42) / -(1+2)

# var b: [4]int;
# var c = 2 + 4 + a;

{
    var a: int;
    a = 4 * 12;
    if a == 3 {
        return foobar();
    }
}
";

fn main() {
    let mut parser = Parser::new(CODE);
    parser.parse();
}
