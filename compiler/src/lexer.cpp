#include "lexer.hpp"
#include <ctype.h>


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


void Lexer::scan() {
    while (m_char) {
        m_tok.row  = m_row;
        m_tok.col  = m_col;
        m_tok.line = m_line;

        char c = next_char();

    }
}
