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
    if (type == N_CALL || type == N_VAR || type == N_VAR_DECL ||
        type == N_DOT || type == N_ARROW) printf(" %s", name.c_str());
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


Node* Parser::parse_expr(TokenType level) {

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
        n->add(parse_expr(T_DOT));
        break;
    case T_NOT:
        next_token();
        n = new Node(N_NOT);
        n->add(parse_expr(T_DOT));
        break;
    case T_AND:
        next_token();
        n = new Node(N_REF);
        n->add(parse_expr(T_DOT));
        break;
    case T_MUL:
        next_token();
        n = new Node(N_DEREF);
        n->add(parse_expr(T_DOT));
        break;
    case T_PARENT:
        next_token();
        // TODO: cast
        n = parse_expr();
        match_token(T_CLOSE_PARENT);
        break;
    case T_ID:
        t = next_token();
        if (m_tok.type == T_PARENT) {
            next_token();
            n = new Node(N_CALL);
            n->name = t.name;
            // arguments
            if (m_tok.type != T_CLOSE_PARENT) {
                for (;;) {
                    n->add(parse_expr());
                    if (m_tok.type == T_CLOSE_PARENT) break;
                    match_token(T_COMMA);
                }
            }
            next_token();
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
            n->add(m);
            n->add(parse_expr(it->second));
            if (it->first == T_BRACKET) {
                match_token(T_CLOSE_BRACKET);
            }
            continue;
        }
        if (m_tok.type == T_DOT ||
            m_tok.type == T_ARROW) {
            TokenType t = m_tok.type;
            // field access
            next_token();
            Token tok = match_token(T_ID);
            Node* m = n;
            n = new Node(NodeType(t));
            n->name = tok.name;
            n->add(m);

            continue;
        }

        printf("%d:%d: parser error: unexpected token %d\n",
            m_tok.row, m_tok.col, m_tok.type);
        exit(1);
    }

    return n;
}


bool Parser::try_parse_data_type(DataType& dt) {
    if (m_tok.type == T_INT) dt.type = DataType::INT;
    else if (m_tok.type == T_VOID) dt.type = DataType::VOID;
    else if (m_tok.type == T_ID) {
        auto it = m_root->structs.find(m_tok.name);
        if (it == m_root->structs.end()) return false;
        dt.type = DataType::STRUCT;
        dt.strct = &it->second;
    }
    else return false;
    next_token();
    while (m_tok.type == T_MUL) {
        next_token();
        ++dt.pointer;
    }
    return true;
};


bool Parser::try_parse_array(DataType& dt) {
    if (m_tok.type != T_BRACKET) return false;
    next_token();
    // XXX: enum
    dt.is_array = true;
    dt.length   = match_token(T_NUMBER).number;
    return true;
}


Node* Parser::parse_stmt() {

    Node* n;
    DataType dt;

    if (try_parse_data_type(dt)) {
        n = new Node(N_VAR_DECL);
        n->data_type = dt;
        n->name = match_token(T_ID).name;
        try_parse_array(n->data_type);
        match_token(T_SEMICOLON);
        return n;
    }

    switch (m_tok.type) {
    case T_BRACE:
        next_token();
        n = new Node(N_BLOCK);
        while (m_tok.type != T_CLOSE_BRACE) {
            n->add(parse_stmt());
        }
        next_token();
        return n;

    case T_WHILE:
        next_token();
        n = new Node(N_WHILE);
        match_token(T_PARENT);
        n->add(parse_expr());
        match_token(T_CLOSE_PARENT);
        n->add(parse_stmt());
        return n;

    case T_IF:
        next_token();
        n = new Node(N_IF);
        match_token(T_PARENT);
        n->add(parse_expr());
        match_token(T_CLOSE_PARENT);
        n->add(parse_stmt());
        if (m_tok.type == T_ELSE) {
            next_token();
            n->add(parse_stmt());
        }
        return n;

    case T_RETURN:
        next_token();
        n = new Node(N_RETURN);
        if (m_tok.type != T_SEMICOLON) n->add(parse_expr());
        match_token(T_SEMICOLON);
        return n;

    default:
        n = parse_expr();
        match_token(T_SEMICOLON);
        return n;
    }
}


RootNode* Parser::parse_program() {
    m_root = new RootNode();

    while (m_tok.type) {

        DataType dt;
        if (try_parse_data_type(dt)) {
            Node* n = new Node(N_VAR_DECL);
            n->data_type = dt;
            n->name = match_token(T_ID).name;
            try_parse_array(n->data_type);
            match_token(T_SEMICOLON);

            m_root->add(n);

            continue;
        }


        if (m_tok.type == T_ENUM) {

            continue;
        }
        if (m_tok.type == T_STRUCT) {

            continue;
        }


        printf("%d:%d: parser error: unexpected token %d\n",
            m_tok.row, m_tok.col, m_tok.type);
        exit(1);
    }


    return m_root;
}
