#[allow(dead_code)]
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

#[allow(dead_code)]
#[derive(Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub pos: usize,
}

pub struct Lexer<'a> {
    code: &'a [u8],
    pos: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(code: &'a str) -> Self {
        Self {
            code: code.as_bytes(),
            pos: 0,
        }
    }

    fn ident_len(&self, pos: usize) -> usize {
        let mut len = 0;
        loop {
            match self.code.get(pos + len) {
                Some(x) if x.is_ascii_alphanumeric() || *x == b'_' => len += 1,
                _ => break,
            }
        }
        len
    }
    fn number_len(&self, pos: usize) -> usize {
        let mut len = 0;
        loop {
            match self.code.get(pos + len) {
                Some(x) if x.is_ascii_digit() => len += 1,
                _ => break,
            }
        }
        len
    }

    pub fn ident_name(&self, tok: &Token) -> &'a str {
        assert_eq!(tok.kind, TokenKind::Ident);
        unsafe {
            std::str::from_utf8_unchecked(&self.code[tok.pos..tok.pos + self.ident_len(tok.pos)])
        }
    }
    pub fn number_value(&self, tok: &Token) -> i32 {
        assert_eq!(tok.kind, TokenKind::Number);
        unsafe {
            std::str::from_utf8_unchecked(&self.code[tok.pos..tok.pos + self.number_len(tok.pos)])
                .parse()
                .unwrap()
        }
    }

    pub fn next_token(&mut self) -> Token {
        // skip whitespace
        loop {
            match self.code.get(self.pos) {
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
        }

        if pos == self.code.len() {
            tok!(EOF);
        }
        self.pos += 1;

        match self.code[pos] {
            b'=' => match self.code.get(self.pos) {
                _ => tok!(Equal),
            },

            b'+' => tok!(Plus),
            b'*' => tok!(Asterisk),

            //...

            // ident
            c if c == b'_' || c.is_ascii_alphabetic() => {
                self.pos += self.ident_len(self.pos);
                tok!(Ident)
            }

            c if c.is_ascii_digit() => {
                self.pos += self.number_len(self.pos);

                tok!(Number)
            }

            _ => tok!(Invalid),
        }
    }
}
