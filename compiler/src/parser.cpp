#include "parser.hpp"
#include <map>
#include <cassert>


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

void RootNode::print() const {
    printf("ENUMS\n");
    for (auto const& p : enums) {
        printf("  %s = %d\n", p.first.c_str(), p.second);
    }
    printf("STRUCTS\n");
    for (auto const& p : structs) {
        printf("  %s\n", p.first.c_str());
        for (Field const& f : p.second.fields) {
            printf("    %s %s\n", f.name.c_str(), f.data_type.to_string().c_str());
        }
    }
    printf("GLOBALS\n");
    for (auto const& p : globals) {
        Field const& f = p.second;
        printf("  %s %s\n", f.name.c_str(), f.data_type.to_string().c_str());
    }

    printf("FUNCTIONS\n");
    for (auto const& p : functions) {
        Function const& f = p.second;
        printf("  %s %s\n", f.name.c_str(), f.return_type.to_string().c_str());
        for (Field const& f : f.params) {
            printf("    %s %s\n", f.name.c_str(), f.data_type.to_string().c_str());
        }
        for (Node const* n : f.stmts) n->print(4);
    }
}

std::string DataType::to_string() const {
    if (type == INVALID) return "INVALID";
    std::string s = type == VOID ? "void"
                  : type == INT ? "int"
                  : strct->name;
    assert(pointer > 0);
    s += std::string(pointer, '*');
    if (is_array) s += '[' + std::to_string(length) + ']';
    return s;
}
int DataType::size() const {
    int s = 1;
    if (pointer == 0 && type == STRUCT) s = strct->size();
    if (is_array) s *= length;
    return s;
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
    std::string name;

    // unary
    switch (m_tok.type) {
    case T_NUMBER:
        n = new Node(N_NUMBER);
        n->number = next_token().number;
        n->data_type = { DataType::INT };
        break;
    case T_SUB:
        next_token();
        n = parse_expr(T_DOT);
        if (n->type == N_NUMBER) n->number = -n->number;
        else {
            assert(n->data_type == DataType{ DataType::INT });
            n = new Node(N_NEG, n);
            n->data_type = { DataType::INT };
        }
        break;
    case T_NOT:
        next_token();
        n = parse_expr(T_DOT);
        if (n->type == N_NUMBER) n->number = !n->number;
        else {
            assert(n->data_type == DataType{ DataType::INT });
            n = new Node(N_NOT, n);
            n->data_type = { DataType::INT };
        }
        break;
    case T_AND:
        next_token();
        n = new Node(N_REF, parse_expr(T_DOT));
        n->data_type = n->kids.front()->data_type;
        ++n->data_type.pointer;
        // TODO: check lvalue
        break;
    case T_MUL:
        next_token();
        n = new Node(N_DEREF, parse_expr(T_DOT));
        n->data_type = n->kids.front()->data_type;
        --n->data_type.pointer;
        assert(n->data_type.pointer >= 0);
        break;
    case T_PARENT:
        next_token();
        // TODO: cast
        n = parse_expr();
        match_token(T_CLOSE_PARENT);
        break;
    case T_ID:
        name = next_token().name;
        if (m_tok.type == T_PARENT) {
            next_token();
            n = new Node(N_CALL);
            n->name = name;
            // arguments
            if (m_tok.type != T_CLOSE_PARENT) {
                for (;;) {
                    n->add(parse_expr());
                    if (m_tok.type == T_CLOSE_PARENT) break;
                    match_token(T_COMMA);
                }
            }
            next_token();

            // find function
            Function f;
            f.name = n->name;
            for (Node* k : n->kids) f.params.push_back({ "", k->data_type });
            std::string sign = f.signature_str();
            printf("looking for function %s\n", sign.c_str());

            assert(m_root->functions.count(sign) > 0);
            n->function = &m_root->functions[sign];
            n->data_type = n->function->return_type;

        }
        else {
            auto it = m_root->enums.find(name);
            if (it != m_root->enums.end()) {
                n = new Node(N_NUMBER);
                n->number = it->second;
                n->data_type = { DataType::INT };
            }
            else {
                n = new Node(N_VAR);
                n->name = name;
                // XXX: lookup

                // find variable

            }
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
            Node* m = parse_expr(it->second);
            // fold constants
            bool fold = false;
            if (n->type == N_NUMBER && m->type == N_NUMBER) {
                fold = true;
                if      (it->first == T_ADD) n->number += m->number;
                else if (it->first == T_SUB) n->number -= m->number;
                else if (it->first == T_MUL) n->number *= m->number;
                else if (it->first == T_DIV) n->number /= m->number;
                else if (it->first == T_SHL) n->number <<= m->number;
                else if (it->first == T_SHR) n->number >>= m->number;
                else if (it->first == T_AND) n->number &= m->number;
                else if (it->first == T_OR)  n->number |= m->number;
                else if (it->first == T_EQ)  n->number = n->number == m->number;
                else if (it->first == T_NE)  n->number = n->number != m->number;
                else if (it->first == T_LT)  n->number = n->number <  m->number;
                else if (it->first == T_LE)  n->number = n->number <= m->number;
                else if (it->first == T_GT)  n->number = n->number >  m->number;
                else if (it->first == T_GE)  n->number = n->number >= m->number;
                else if (it->first == T_AND) n->number = n->number && m->number;
                else if (it->first == T_OR)  n->number = n->number || m->number;
                else fold = false;
            }
            if (fold) {
                delete m;
                n->data_type = { DataType::INT };
            }
            else n = new Node(NodeType(it->first), n, m);
            if (it->first == T_BRACKET) {
                match_token(T_CLOSE_BRACKET);
            }
        }
        else if (m_tok.type == T_DOT ||
            m_tok.type == T_ARROW) {
            TokenType t = m_tok.type;
            // field access
            next_token();
            Token tok = match_token(T_ID);
            Node* m = n;
            n = new Node(NodeType(t));
            n->name = tok.name;
            n->add(m);
        }
        else {
            printf("%d:%d: parser error: unexpected token %d\n",
                m_tok.row, m_tok.col, m_tok.type);
            exit(1);
        }
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
    dt.pointer = 0;
    while (m_tok.type == T_MUL) {
        next_token();
        ++dt.pointer;
    }
    return true;
}


bool Parser::try_parse_array(DataType& dt) {
    if (m_tok.type != T_BRACKET) {
        dt.is_array = false;
        dt.length   = 0;
        return false;
    }
    next_token();
    dt.is_array = true;
    Node* n = parse_expr(T_LOGIC_OR);
    match_token(T_CLOSE_BRACKET);
    assert(n->type == N_NUMBER);
    dt.length = n->number;
    delete n;
    return true;
}


Node* Parser::parse_stmt() {

    Node* n;
    DataType dt;

    if (try_parse_data_type(dt)) {
        std::string name = match_token(T_ID).name;
        try_parse_array(dt);
        match_token(T_SEMICOLON);
        n = new Node(N_VAR_DECL);
        n->name = name;
        n->data_type = dt;
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
        match_token(T_PARENT);
        n = new Node(N_WHILE, parse_expr());
        match_token(T_CLOSE_PARENT);
        n->add(parse_stmt());
        return n;

    case T_IF:
        next_token();
        match_token(T_PARENT);
        n = new Node(N_IF, parse_expr());
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
            std::string name = match_token(T_ID).name;
            if (m_tok.type == T_PARENT) {
                // function

                Function f;
                f.name = name;
                f.return_type = dt;

                next_token();
                if (m_tok.type != T_CLOSE_PARENT) {
                    for (;;) {
                        assert(try_parse_data_type(dt));
                        name = match_token(T_ID).name;
                        f.params.push_back({ name, dt });
                        if (m_tok.type == T_CLOSE_PARENT) break;
                        match_token(T_COMMA);
                    }
                }
                next_token();

                if (m_tok.type == T_SEMICOLON) {
                    // declaration
                    next_token();
                }
                else {
                    // definition
                    match_token(T_BRACE);
                    while (m_tok.type != T_CLOSE_BRACE) {
                        f.stmts.push_back(parse_stmt());
                    }
                    next_token();
                    assert(m_root->functions.count(f.signature_str()) == 0);
                    //m_root->functions[f.signature_str()] = std::move(f);
                    std::swap(m_root->functions[f.signature_str()], f);
                }
            }
            else {
                // variable declaration
                try_parse_array(dt);
                match_token(T_SEMICOLON);
                assert(m_root->globals.count(name) == 0);
                Field& g = m_root->globals[name];
                g.name = name;
                g.data_type = dt;
            }
        }
        else if (m_tok.type == T_STRUCT) {
            next_token();
            std::string name = match_token(T_ID).name;
            assert(m_root->enums.count(name) == 0);
            Struct& s = m_root->structs[name];
            if (m_tok.type == T_SEMICOLON) {
                next_token();
            }
            else {
                assert(!s.is_defined);
                s.is_defined = true;
                s.name = name;
                match_token(T_BRACE);
                DataType dt;
                while (try_parse_data_type(dt)) {
                    std::string name = match_token(T_ID).name;
                    try_parse_array(dt);
                    match_token(T_SEMICOLON);
                    s.fields.push_back({ name, dt });
                }
                match_token(T_CLOSE_BRACE);
                match_token(T_SEMICOLON);
            }
        }
        else if (m_tok.type == T_ENUM) {
            next_token();
            match_token(T_BRACE);

            int i = 0;
            for (;;) {
                if (m_tok.type == T_ID) {
                    std::string name = next_token().name;
                    assert(m_root->enums.count(name) == 0);
                    assert(m_root->structs.count(name) == 0);
                    if (m_tok.type == T_ASSIGN) {
                        next_token();
                        Node* n = parse_expr(T_LOGIC_OR);
                        assert(n->type == N_NUMBER);
                        i = n->number;
                        delete n;
                    }
                    m_root->enums[name] = i++;
                    if (m_tok.type == T_COMMA) {
                        next_token();
                        continue;
                    }
                }
                break;
            }
            match_token(T_CLOSE_BRACE);
            match_token(T_SEMICOLON);
        }
        else {
            printf("%d:%d: parser error: unexpected token %d\n",
                m_tok.row, m_tok.col, m_tok.type);
            exit(1);
        }
    }


    return m_root;
}
