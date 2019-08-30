#include "lexer.hpp"
#include <cstring>
#include <map>


char Lexer::next_char() {
    char c = m_char;
    if ((m_char = *m_code)) ++m_code;
    if (m_char == '\n') {
        m_line = m_code;
        ++m_row;
        m_col = 1;
    }
    else if (m_char) {
        ++m_col;
    }
    return c;
}


const std::map<std::string, TokenType> KEYWORDS = {
    { "return",   T_RETURN },
    { "if",       T_IF },
    { "else",     T_ELSE },
    { "while",    T_WHILE },
    { "for",      T_FOR },
    { "break",    T_BREAK },
    { "continue", T_CONTINUE },
    { "enum",     T_ENUM },
};


TokenType Lexer::scan() {
    while (m_char) {
        m_tok.row  = m_row;
        m_tok.col  = m_col;
        m_tok.line = m_line;

        char c = next_char();

        if (c == '#') {
            while (m_char && m_char != '\n') next_char();
            continue;
        }

        if (isalpha(c) || c == '_') {
            m_tok.name = c;
            while (isalnum(m_char) || c == '_') m_tok.name += next_char();
            auto it = KEYWORDS.find(m_tok.name);
            if (it != KEYWORDS.end()) return it->second;
            return T_ID;
        }

        if (isdigit(c)) {
            m_tok.number = c - '0';
            while (isdigit(m_char)) m_tok.number = m_tok.number * 10 + next_char() - '0';
            return T_NUMBER;
        }

        if (strchr("[](){}+-*/%;~,", c)) return TokenType(c);
        if (c == '=' || c == '!') {
            if (m_char != '=') return TokenType(c);
            return TokenType(c + 0x100);
        }

        // TODO: shift

        if (!isspace(c)) {
            printf("%d:%d: lexer error: unexpected character '%c'\n", m_tok.row, m_tok.col, c);
            exit(0);
        }
    }


    return T_EOF;
}
