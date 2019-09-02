#include "lexer.hpp"
#include <vector>
#include <map>


#define GENERATE_ENUM(e, ...) e __VA_ARGS__,
#define FOR_EACH_NodeType(F) \
    F(N_ROOT) \
    F(N_STRUCT) \
    F(N_FUNC) \
    F(N_VAR_DECL) \
    /* stmt */\
    F(N_BLOCK) \
    F(N_RETURN) \
    F(N_IF) \
    F(N_WHILE) \
    /* prefix */\
    F(N_VAR) \
    F(N_CALL) \
    F(N_NUMBER) \
    F(N_NEG) \
    F(N_NOT) \
    F(N_REF) \
    F(N_DEREF) \
    /* infix */\
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
    F(N_ARROW) \
    F(N_BRACKET) \


enum NodeType {
    FOR_EACH_NodeType(GENERATE_ENUM)
};


struct Struct;


struct DataType {
    enum Type {
        VOID,
        INT,
        STRUCT,
    };
    Type    type;
    int     pointer;
    bool    is_array;
    int     length;
    Struct* strct;
};


struct Struct {
    std::string name;
    struct Field {
        std::string name;
        DataType    data_type;
    };
};


struct Node {
    Node(NodeType type) : type(type) {}
    ~Node() {
        for (Node const* k : kids) delete k;
    }


    void print(int indent = 0) const;
    void add(Node* kid) {
        kids.push_back(kid);
        kid->parent = this;
    }

    NodeType           type;
    Node*              parent = nullptr;
    std::vector<Node*> kids;

    std::string        name;
    int                number;
    DataType           data_type = {};

};


struct RootNode : Node {
    RootNode() : Node(N_ROOT) {}

    std::map<std::string, int>    enums;
    std::map<std::string, Struct> structs;
};



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
