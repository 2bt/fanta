#[derive(PartialEq, Debug)]
pub enum TokenKind {
    Invalid,

    EOF,
    Ident,
    Number,
    String,

    Plus,
    Minus,
    Asterisk,
    Slash,
    Percent,
    BitOr,
    BitAnd,
    BitXor,
    LeftShift,
    RightShift,

    Equal,
    NotEqual,
    Less,
    LessOrEqual,
    Greater,
    GreaterOrEqual,

    Or,
    And,
    Bang,

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

    Dot,
    Semicolon,
    Comma,
    Colon,

    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    LeftBrace,
    RightBrace,
}

#[derive(Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub pos: usize,
}

pub struct Lexer<'a> {
    code: &'a str,
    pos: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(code: &'a str) -> Self {
        Self { code, pos: 0 }
    }

    fn ident_len(&self, pos: usize) -> usize {
        let mut len = 0;
        loop {
            match self.code.as_bytes().get(pos + len) {
                Some(x) if x.is_ascii_alphanumeric() || *x == b'_' => len += 1,
                _ => break,
            }
        }
        len
    }
    fn number_len(&self, pos: usize) -> usize {
        let mut len = 0;
        loop {
            match self.code.as_bytes().get(pos + len) {
                Some(x) if x.is_ascii_digit() => len += 1,
                _ => break,
            }
        }
        len
    }

    pub fn ident_name(&self, tok: &Token) -> &'a str {
        assert_eq!(tok.kind, TokenKind::Ident);
        self.code
            .get(tok.pos..tok.pos + self.ident_len(tok.pos))
            .unwrap()
    }
    pub fn number_value(&self, tok: &Token) -> i32 {
        assert_eq!(tok.kind, TokenKind::Number);
        self.code
            .get(tok.pos..tok.pos + self.number_len(tok.pos))
            .unwrap()
            .parse()
            .unwrap()
    }
    pub fn string_value(&self, tok: &Token) -> String {
        assert_eq!(tok.kind, TokenKind::String);
        let mut bytes = vec![];

        let code = self.code.as_bytes();
        assert_eq!(code.get(tok.pos), Some(&b'"'));

        let mut pos = tok.pos + 1;
        loop {
            match code.get(pos) {
                Some(b'"') => break,
                Some(b'\\') => {
                    bytes.push(code[pos + 1]);
                    pos += 2;
                }
                Some(x) => {
                    bytes.push(*x);
                    pos += 1;
                }
                _ => unreachable!(),
            }
        }
        String::from_utf8(bytes).unwrap()
    }

    pub fn next_token(&mut self) -> Token {
        let code = self.code.as_bytes();
        loop {
            match code.get(self.pos) {
                Some(b'#') => loop {
                    match code.get(self.pos) {
                        None | Some(b'\n') => break,
                        _ => self.pos += 1,
                    }
                },
                Some(x) if x.is_ascii_whitespace() => self.pos += 1,
                _ => break,
            }
        }
        let pos = self.pos;

        macro_rules! tok {
            ($t:tt) => {
                return Token {
                    kind: TokenKind::$t,
                    pos,
                }
            };
            ($t:tt, $len:expr) => {{
                self.pos += $len;
                tok!($t)
            }};
        }

        if pos >= code.len() {
            tok!(EOF);
        }
        self.pos += 1;

        let n = code.get(pos + 1);
        let m = code.get(pos + 2);
        match code[pos] {
            b'=' => match n {
                Some(b'=') => tok!(Equal, 1),
                _ => tok!(Assign),
            },
            b'<' => match n {
                Some(b'<') => match m {
                    Some(b'=') => tok!(LeftShiftAssign, 2),
                    _ => tok!(LeftShift, 1),
                },
                Some(b'=') => tok!(LessOrEqual, 1),
                _ => tok!(Less),
            },
            b'>' => match n {
                Some(b'>') => match m {
                    Some(b'=') => tok!(RightShiftAssign, 2),
                    _ => tok!(RightShift, 1),
                },
                Some(b'=') => tok!(GreaterOrEqual, 1),
                _ => tok!(Greater),
            },
            b'!' => match n {
                Some(b'=') => tok!(NotEqual, 1),
                _ => tok!(Bang),
            },
            b'+' => match n {
                Some(b'=') => tok!(PlusAssign, 1),
                _ => tok!(Plus),
            },
            b'-' => match n {
                Some(b'=') => tok!(MinusAssign, 1),
                _ => tok!(Minus),
            },
            b'*' => match n {
                Some(b'=') => tok!(AsteriskAssign, 1),
                _ => tok!(Asterisk),
            },
            b'/' => match n {
                Some(b'=') => tok!(SlashAssign, 1),
                _ => tok!(Slash),
            },
            b'%' => match n {
                Some(b'=') => tok!(PercentAssign, 1),
                _ => tok!(Percent),
            },
            b'|' => match n {
                Some(b'|') => tok!(Or, 1),
                Some(b'=') => tok!(BitOrAssign, 1),
                _ => tok!(BitOr),
            },
            b'&' => match n {
                Some(b'&') => tok!(And, 1),
                Some(b'=') => tok!(BitAndAssign, 1),
                _ => tok!(BitAnd),
            },
            b'^' => match n {
                Some(b'=') => tok!(BitXorAssign, 1),
                _ => tok!(BitXor),
            },
            b'.' => tok!(Dot),
            b',' => tok!(Comma),
            b':' => tok!(Colon),
            b';' => tok!(Semicolon),
            b'(' => tok!(LeftParen),
            b')' => tok!(RightParen),
            b'[' => tok!(LeftBracket),
            b']' => tok!(RightBracket),
            b'{' => tok!(LeftBrace),
            b'}' => tok!(RightBrace),
            c if c == b'_' || c.is_ascii_alphabetic() => tok!(Ident, self.ident_len(self.pos)),
            c if c.is_ascii_digit() => tok!(Number, self.number_len(self.pos)),
            b'"' => {
                loop {
                    let c = code.get(self.pos);
                    match c {
                        None => tok!(Invalid),
                        Some(b'"') => {
                            self.pos += 1;
                            break;
                        }
                        Some(b'\\') => self.pos += 2,
                        Some(_) => self.pos += 1,
                    }
                }
                self.pos += 1;
                tok!(String)
            }
            _ => tok!(Invalid),
        }
    }
}
