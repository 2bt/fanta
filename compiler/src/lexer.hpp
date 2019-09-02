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
    T_STRUCT,
    T_VOID,
    T_INT,

    T_ID,
    T_NUMBER,

    T_PARENT,
    T_CLOSE_PARENT,
    T_CLOSE_BRACKET,
    T_BRACE,
    T_CLOSE_BRACE,

    T_COMMA,
    T_SEMICOLON,
    T_COLON,
    T_NOT,

    // binary operators
    T_ASSIGN,
    T_LOGIC_OR,
    T_LOGIC_AND,
    T_OR,
    T_AND,
    T_EQ,
    T_NE,
    T_LT,
    T_GT,
    T_LE,
    T_GE,
    T_SHL,
    T_SHR,
    T_ADD,
    T_SUB,
    T_DIV,
    T_MUL,
    T_MOD,
    T_DOT,
    T_ARROW,
    T_BRACKET,


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

    Token next_token();

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


