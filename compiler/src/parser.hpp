#include "lexer.hpp"
#include <vector>
#include <map>


#define GENERATE_ENUM(e, ...) e __VA_ARGS__,
#define FOR_EACH_NodeType(F) \
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
    std::string to_string() const;
    int size() const;
    enum Type { VOID, INT, STRUCT };
    Type    type     = VOID;
    int     pointer  = 0;
    bool    is_array = false;
    int     length   = 0;
    Struct* strct    = nullptr;
};


struct Node {
    Node(NodeType type) : type(type) {}
    Node(NodeType type, Node* kid) : Node(type) { add(kid); }
    Node(NodeType type, Node* kid, Node* kid2) : Node(type, kid) { add(kid2); }
    ~Node() { for (Node const* k : kids) delete k; }

    void print(int indent = 0) const;
    void add(Node* kid) { kids.push_back(kid); }

    NodeType           type;
    std::vector<Node*> kids;

    std::string        name;
    int                number;
    DataType           data_type;
};



struct Field {
    std::string name;
    DataType    data_type;
};


struct Struct {
    int size() const {
        int s = 0;
        for (auto const& f : fields) s += f.data_type.size();
        return s;
    }

    std::string        name;
    std::vector<Field> fields;
};

struct Function {
    ~Function() { for (Node const* n : stmts) delete n; }

    std::string        name;
    DataType           data_type;
    std::vector<Field> params;
    std::vector<Node*> stmts;
};


struct RootNode {
    ~RootNode() {
        //for (Node const* k : kids) delete k;
    }
    void print() const;

    std::map<std::string, int>      enums;
    std::map<std::string, Struct>   structs;
    std::map<std::string, Function> functions;
    std::map<std::string, Field>    globals;

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
