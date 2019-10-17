#pragma once

#include "lexer.hpp"
#include "ast.hpp"



class Parser {
public:
    Parser(char const* code) : m_lexer(code) {
        next_token();
    }

    RootNode* parse_program();

private:
    Token next_token();
    Token match_token(TokenType type);
    Node* parse_expr(TokenType level = T_ASSIGN);
    Node* parse_stmt();
    bool  try_parse_data_type(DataType& dt);
    bool  try_parse_array(DataType& dt);

    RootNode* m_root;
    Lexer     m_lexer;
    Token     m_tok;
};
