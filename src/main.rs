mod lexer;
use lexer::*;
use std::fmt::Write;

#[allow(dead_code)]
#[derive(Debug)]
enum OperationKind {
    // infix
    Assign,
    PlusAssign,
    MinusAssign,
    AsteriskAssign,
    SlashAssign,
    PercentAssign,
    BitOrAssign,
    BitAndAssign,
    BitXorAssign,
    LeftShiftAssign,
    RightShiftAssign,

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
        use TokenKind::*;
        Some(match t {
            Plus => Add,
            Minus => Sub,
            Asterisk => Mul,
            Slash => Div,
            Percent => Mod,
            // ...
            _ => return None,
        })
    }
    fn precedence(&self) -> usize {
        use OperationKind::*;
        match self {
            Assign | PlusAssign | MinusAssign | AsteriskAssign | SlashAssign | PercentAssign
            | BitOrAssign | BitAndAssign | BitXorAssign | LeftShiftAssign | RightShiftAssign => 1,
            Or => 2,
            And => 3,
            BitOr => 4,
            BitXor => 5,
            BitAnd => 6,
            Equal | NotEqual => 7,
            Less | LessEqual | Greater | GreaterEqual => 8,
            LeftShift | RightShift => 9,
            Add | Sub => 10,
            Mul | Div | Mod => 11,
            _ => 12,
        }
    }
}

struct Operation {
    kind: OperationKind,
    x: usize,
    y: Option<usize>,
}

enum Expr {
    Number(i32),
    Operation(Operation),
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

#[allow(dead_code)]
struct Parser<'a> {
    lexer: Lexer<'a>,
    token: Token,

    datatypes: Vec<DataType>,
    structs: Vec<Struct>,
    // constants
    // globals
    functions: Vec<Function>,
    nodes: Vec<Expr>,
}

#[allow(dead_code)]
#[derive(Debug)]
enum ParserError {
    Token(Token),
    Expect(Token, TokenKind),
}

impl<'a> Parser<'a> {
    fn new(code: &'a str) -> Self {
        Parser {
            lexer: Lexer::new(code),
            token: Token {
                kind: TokenKind::Invalid,
                pos: 0,
            },
            datatypes: Default::default(),
            structs: Default::default(),
            functions: Default::default(),
            nodes: Default::default(),
        }
    }

    fn next_token(&mut self) -> Token {
        let tok = self.token;
        self.token = self.lexer.next_token();
        tok
    }

    fn expect(&mut self, t: TokenKind) -> Result<Token, ParserError> {
        let tok = self.next_token();
        if tok.kind != t {
            return Err(ParserError::Expect(tok, t));
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

    fn parse_datatype(&mut self) -> Result<usize, ParserError> {
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
                    .map_err(|_| ParserError::Token(tok))?;
                self.expect(TokenKind::RightBracket)?;
                let datatype = self.parse_datatype()?;
                self.add_datatype(DataType::Array { len, datatype })
            }
            _ => return Err(ParserError::Token(tok)),
        })
    }

    fn add_node(&mut self, e: Expr) -> usize {
        let n = self.nodes.len();
        self.nodes.push(e);
        n
    }

    fn parse_expr(&mut self, precedence: usize) -> Result<usize, ParserError> {
        use TokenKind::*;
        let tok = self.next_token();
        let mut n = match tok.kind {
            Number => self.add_node(Expr::Number(self.lexer.number_value(&tok))),
            LeftParen => {
                let n = self.parse_expr(0)?;
                self.expect(RightParen)?;
                n
            }
            Minus => {
                let n = self.parse_expr(999)?;
                if let Expr::Number(x) = &mut self.nodes[n] {
                    *x = -*x;
                    n
                } else {
                    self.add_node(Expr::Operation(Operation {
                        kind: OperationKind::Neg,
                        x: n,
                        y: None,
                    }))
                }
            }
            // TODO
            _ => return Err(ParserError::Token(tok)),
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
            n = self.add_node(Expr::Operation(Operation {
                x: n,
                y: Some(m),
                kind: op,
            }));
        }
        Ok(n)
    }

    fn parse(&mut self) {
        self.next_token();

        let e = self.parse_datatype().unwrap();
        let f = self.parse_datatype().unwrap();

        println!("{}", self.datatype_to_string(e));
        println!("{}", self.datatype_to_string(f));

        let root = self.parse_expr(0).unwrap();
        println!("{}", self.expr_to_string(root));
    }

    fn fmt_expr(&self, n: usize, f: &mut String) {
        use OperationKind::*;
        match &self.nodes[n] {
            Expr::Number(x) => write!(f, "{x}").unwrap(),
            Expr::Operation(o) => match o.kind {
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
                            _ => "?",
                        }
                    )
                    .unwrap();
                    self.fmt_expr(o.y.unwrap(), f);
                    write!(f, ")").unwrap();
                }
            },
            _ => write!(f, "?").unwrap(),
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
            DataType::Struct { strct } => {
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
}

const CODE: &str = "

int

**[4]*int


3 * (1 + 2) - (3) -

(3 + 4) * -(42) / -(1+2)

#1 + 2 * 3 + 4 * 5 * 6 + 7 / 2*2 -

";

fn main() {
    let mut parser = Parser::new(CODE);
    parser.parse();
}
