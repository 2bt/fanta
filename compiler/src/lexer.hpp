#include <string>

enum TokenType {
    T_EOF,

    // keywords
    T_RETURN,
    T_IF,
    T_ELSE,
    T_WHILE,
    T_FOR,
    T_BREAK,
    T_CONTINUE,
    T_ENUM,

    T_ID,
    T_NUMBER,

};

struct Token {
    TokenType   type;
    int         row;
    int         col;
    char const* line;
    std::string name;
    int         number;
};


class Lexer {
public:
    Lexer(char const* code) : m_code(code), m_line(code) {
        next_char();
    }

    Token next_token() {
        m_tok.type = scan();
        return m_tok;
    }

private:
    char next_char();
    TokenType scan();

    char const* m_code;
    char        m_char;

    int         m_row = 1;
    int         m_col = 1;
    char const* m_line;

    Token       m_tok = {};
};


