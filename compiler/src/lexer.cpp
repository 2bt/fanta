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


Token Lexer::next_token() {
    m_tok.type = scan();
    return m_tok;
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
    { "struct",   T_STRUCT },
    { "void",     T_VOID },
    { "int",      T_INT },
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

        if (c == '(') return T_PARENT;
        if (c == ')') return T_CLOSE_PARENT;
        if (c == '[') return T_BRACKET;
        if (c == ']') return T_CLOSE_BRACKET;
        if (c == '{') return T_BRACE;
        if (c == '}') return T_CLOSE_BRACE;
        if (c == ',') return T_COMMA;
        if (c == ';') return T_SEMICOLON;
        if (c == ':') return T_COLON;
        if (c == '!') return T_NOT;
        if (c == '+') return T_ADD;
        if (c == '*') return T_MUL;
        if (c == '/') return T_DIV;
        if (c == '%') return T_MOD;
        if (c == '.') return T_DOT;
        if (c == '-') {
            if (m_char == '>') { next_char(); return T_ARROW; }
            return T_SUB;
        }
        if (c == '=') {
            if (m_char == '=') { next_char(); return T_EQ; }
            return T_ASSIGN;
        }
        if (c == '|') {
            if (m_char == '|') { next_char(); return T_LOGIC_OR; }
            return T_OR;
        }
        if (c == '&') {
            if (m_char == '&') { next_char(); return T_LOGIC_AND; }
            return T_AND;
        }
        if (c == '<') {
            if (m_char == '<') { next_char(); return T_SHL; }
            if (m_char == '=') { next_char(); return T_LE; }
            return T_LT;
        }
        if (c == '>') {
            if (m_char == '>') { next_char(); return T_SHR; }
            if (m_char == '=') { next_char(); return T_GE; }
            return T_GT;
        }
        if (c == '!') {
            if (m_char == '=') { next_char(); return T_NE; }
            return T_NOT;
        }

        if (!isspace(c)) {
            printf("%d:%d: lexer error: unexpected character '%c'\n", m_tok.row, m_tok.col, c);
            exit(1);
        }
    }


    return T_EOF;
}
