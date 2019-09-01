#include "parser.hpp"

constexpr char const* NODE_TYPE_STRING_TABLE[] = {
    "NUMBER",
    "NEG",
    "NOT",
    "CALL",
    "USE",
};

void Node::print(int indent) const {
    printf("%*s%s", indent, "", NODE_TYPE_STRING_TABLE[type]);
    if (type == N_NUMBER) printf(" %d", number);
    if (type == N_CALL || type == N_USE) printf(" %s", name.c_str());
    printf("\n");
    for (Node const* k : kids) k->print(indent + 2);
}


Token Parser::next_token() {
    Token t = m_tok;
    m_tok = m_lexer.next_token();
    return t;
}


Token Parser::match_token(TokenType type) {
    if (type != m_tok.type) {
        printf("%d:%d: parser error: unexpected token %d (should probably be %d)\n",
            m_tok.row, m_tok.col,
            m_tok.type, type);
        exit(1);
    }
    return next_token();
}

Node* Parser::expr(int level) {

    Node* n = nullptr;
    Token t;

    // unary
    switch (m_tok.type) {
    case T_NUMBER:
        n = new Node(N_NUMBER);
        n->number = next_token().number;
        break;
    case T_SUB:
        next_token();
        n = new Node(N_NEG);
        n->kids.push_back(expr(99));
        break;
    case T_NOT:
        next_token();
        n = new Node(N_NOT);
        n->kids.push_back(expr(99));
        break;
    case T_PARENT:
        next_token();
        n = expr();
        match_token(T_CLOSE_PARENT);
        break;
    case T_ID:
        t = next_token();
        if (m_tok.type == T_PARENT) {
            next_token();
            n = new Node(N_CALL);
            n->name = t.name;
            // TODO: args
            match_token(T_CLOSE_PARENT);
        }
        else {
            n = new Node(N_USE);
            n->name = t.name;
        }
        break;
    default:
        printf("%d:%d: parser error: unexpected token %d\n",
            m_tok.row, m_tok.col, m_tok.type);
        exit(1);
    }


    // infix


    return n;
}
