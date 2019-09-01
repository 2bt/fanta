#include "lexer.hpp"
#include <vector>


enum NodeType {
    N_NUMBER,
    N_NEG,
    N_NOT,
    N_CALL,
    N_USE,
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

    Node* expr(int level = 0);

private:
    Token next_token();
    Token match_token(TokenType type);

    Lexer m_lexer;
    Token m_tok;
};
