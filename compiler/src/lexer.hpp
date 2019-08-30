
enum TokenType {
    T_EOF,
};

struct Token {
    TokenType   type;
    int         row;
    int         col;
    char const* line;
};


class Lexer {
public:
    Lexer(char const* code) : m_code(code), m_line(code) {}

    Token next_token() {
        scan();
        return m_tok;
    }

private:
    char next_char();
    void scan();

    char const* m_code;
    char        m_char;

    int         m_row = 1;
    int         m_col = 1;
    char const* m_line;

    Token       m_tok = {};
};


