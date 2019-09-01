#include "lexer.hpp"
#include <vector>

#define GENERATE_ENUM(e, ...) e __VA_ARGS__,
#define FOR_EACH_NodeType(F) \
    /* stmt */\
    F(N_BLOCK) \
    F(N_RETURN) \
    F(N_IF) \
    F(N_WHILE) \
    /* expr */\
    F(N_VAR) \
    F(N_CALL) \
    F(N_NUMBER) \
    F(N_NEG) \
    F(N_NOT) \
    /* binary operator */\
    F(N_ASSIGN, = T_ASSIGN) \
    F(N_LOGIC_OR) \
    F(N_LOGIC_AND) \
    F(N_OR) \
    F(N_AND) \
    F(N_EQ) \
    F(N_NE) \
    F(N_LT) \
    F(N_GT) \
    F(N_LE) \
    F(N_GE) \
    F(N_SHL) \
    F(N_SHR) \
    F(N_ADD) \
    F(N_SUB) \
    F(N_DIV) \
    F(N_MUL) \
    F(N_MOD) \
    F(N_DOT) \
    F(N_BRACKET) \


enum NodeType {
    FOR_EACH_NodeType(GENERATE_ENUM)
};


struct Node {
    Node(NodeType type) : type(type) {}
    ~Node() {
        for (Node const* k : kids) delete k;
    }


    void print(int indent = 0) const;

    NodeType           type;
    Node*              parent = nullptr;
    std::vector<Node*> kids;

    std::string        name;
    int                number;
};


class Parser {
public:
    Parser(char const* code) : m_lexer(code) {
        next_token();
    }

    Node* expr(TokenType level = T_ASSIGN);
    Node* stmt();

private:
    Token next_token();
    Token match_token(TokenType type);

    Lexer m_lexer;
    Token m_tok;
};
