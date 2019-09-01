#include "parser.hpp"
#include <map>

#define GENERATE_CASE(e, ...) case e: return #e + 2;
static char const* NodeTypeStr(NodeType t) {
    switch (t) {
    FOR_EACH_NodeType(GENERATE_CASE)
    default: return nullptr;
    }
}

void Node::print(int indent) const {
    printf("%*s%s", indent, "", NodeTypeStr(type));
    if (type == N_NUMBER) printf(" %d", number);
    if (type == N_CALL || type == N_VAR || type == N_DOT) printf(" %s", name.c_str());
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


Node* Parser::expr(TokenType level) {

    Node* n;
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
        n->kids.push_back(expr(T_DOT));
        break;
    case T_NOT:
        next_token();
        n = new Node(N_NOT);
        n->kids.push_back(expr(T_DOT));
        break;
    case T_PARENT:
        next_token();
        // TODO: cast
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
            n = new Node(N_VAR);
            n->name = t.name;
        }
        break;
    default:
        printf("%d:%d: parser error: unexpected token %d\n",
            m_tok.row, m_tok.col, m_tok.type);
        exit(1);
    }


    // infix
    while (m_tok.type >= level) {
        static const std::map<TokenType, TokenType> LEVEL_TABLE = {
            { T_ASSIGN, T_ASSIGN },
            { T_LOGIC_OR, T_LOGIC_AND },
            { T_LOGIC_AND, T_OR },
            { T_OR, T_AND },
            { T_AND, T_EQ },
            { T_EQ, T_LT },
            { T_NE, T_LT },
            { T_LT, T_SHL },
            { T_GT, T_SHL },
            { T_LE, T_SHL },
            { T_GE, T_SHL },
            { T_SHL, T_ADD },
            { T_SHR, T_ADD },
            { T_ADD, T_MUL },
            { T_SUB, T_MUL },
            { T_MUL, T_DOT },
            { T_DIV, T_DOT },
            { T_MOD, T_DOT },
            { T_BRACKET, T_ASSIGN },
        };
        auto it = LEVEL_TABLE.find(m_tok.type);
        if (it != LEVEL_TABLE.end()) {
            next_token();
            Node* m = n;
            n = new Node(NodeType(it->first));
            n->kids.push_back(m);
            n->kids.push_back(expr(it->second));
            if (it->first == T_BRACKET) {
                match_token(T_CLOSE_BRACKET);
            }
            continue;
        }
        if (m_tok.type == T_DOT) {
            // field access
            next_token();
            if (m_tok.type != T_ID) {
                printf("%d:%d: parser error: unexpected token %d\n",
                    m_tok.row, m_tok.col, m_tok.type);
                exit(1);
            }
            Node* m = n;
            n = new Node(NodeType(T_DOT));
            n->name = m_tok.name;
            n->kids.push_back(m);
            next_token();

            continue;
        }

        printf("%d:%d: parser error: unexpected token %d\n",
            m_tok.row, m_tok.col, m_tok.type);
        exit(1);
    }

    return n;
}


Node* Parser::stmt() {

    Node* n = nullptr;

    switch (m_tok.type) {
    case T_BRACE:
        next_token();
        n = new Node(N_BLOCK);
        while (m_tok.type != T_CLOSE_BRACE) {
            n->kids.push_back(stmt());
        }
        next_token();
        break;

    case T_WHILE:
        next_token();
        n = new Node(N_WHILE);
        match_token(T_PARENT);
        n->kids.push_back(expr());
        match_token(T_CLOSE_PARENT);
        n->kids.push_back(stmt());
        break;

    case T_IF:
        next_token();
        n = new Node(N_IF);
        match_token(T_PARENT);
        n->kids.push_back(expr());
        match_token(T_CLOSE_PARENT);
        n->kids.push_back(stmt());
        if (m_tok.type == T_ELSE) {
            next_token();
            n->kids.push_back(stmt());
        }
        break;

    case T_RETURN:
        next_token();
        n = new Node(N_RETURN);
        if (m_tok.type != T_SEMICOLON) n->kids.push_back(expr());
        match_token(T_SEMICOLON);
        break;

    default:
        n = expr();
        match_token(T_SEMICOLON);
        break;
    }

    return n;
}
