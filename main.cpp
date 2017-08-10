// vim: et ts=4 sts=4 sw=4
// a simple math expression evaluator to teach me TDOP parsing
#include <cstdio>
#include <cmath>
#include <cstring>
#include <array>
#include <vector>
#include <string>



class Lexer {
public:
    enum Token {
        NUM = 128,
        ASGN, OR, XOR, AND, // TODO
        EQU, NEQ, LTN, GTN, LEQ, GEQ, SHL, SHR, ADD, SUB, MUL, DIV, MOD, NOP
    };

    Lexer(const char* pos) : m_pos(pos) { }
    void next() { m_tok = scan(); }
    int scan() {
        while (char c = *m_pos) {
            ++m_pos;
            if (isdigit(c)) {
                m_num = c - '0';
                while (isdigit(*m_pos)) m_num = m_num * 10 + *m_pos++ - '0';
                return NUM;
            }
            if (c == '+') return ADD;
            if (c == '-') return SUB;
            if (c == '*') return MUL;
            if (c == '/') return DIV;
            if (c == '%') return MOD;
            if (c == '<') return *m_pos == '<' ? ++m_pos, SHL :
                                 *m_pos == '=' ? ++m_pos, LEQ : LTN;
            if (c == '>') return *m_pos == '>' ? ++m_pos, SHR :
                                 *m_pos == '=' ? ++m_pos, GEQ : GTN;
            if (c == '!') return *m_pos == '=' ? ++m_pos, NEQ : c;
            if (c == '=') return *m_pos == '=' ? ++m_pos, EQU : ASGN;
            if (strchr("()~", c)) return c;
            if (!isspace(c)) {
                printf("unexpected character\n");
                exit(-1);
            }
        }
        return 0;
    }
protected:
    const char* m_pos;
    int         m_tok;
    int         m_num;
    std::string m_name;
};



struct Ast {

    enum Type {
        Invalid,
        Num,
        Add,
        Sub,
        Neg,
        Not,
        Mul,
        Div,
        Mod,

        Call,
        Asgn,
        Ret,
    };

    Type             type;
    std::vector<Ast> kids;
    int              number;
    std::string      name;

    Ast() {}
    Ast(int n) : type(Num), number(n) {}
    Ast(Type t) : type(t) {}
    Ast& add(Ast&& a) {
        kids.emplace_back(std::move(a));
        return *this;
    }

    void print() const {
        printf("(%c", "?#+-_!*/%"[type]);
        if (type == Num) printf("%d", number);
        for (const Ast& a : kids) a.print();
        printf(")");
    }

};


struct Function {
    std::string name;
};

struct Program {
    std::vector<Function> funcs;

};



class Parser : public Lexer {
public:
    Parser(char* pos) : Lexer(pos) { next(); }
    Ast expr(int level = EQU) {
        Ast node;
        if      (m_tok == NUM) { next(); node = Ast(m_num); }
        else if (m_tok == ADD) { next(); node = expr(NOP); }
        else if (m_tok == SUB) { next(); node = Ast(Ast::Type::Neg).add(expr(NOP)); }
        else if (m_tok == '(') { next(); node = expr(); match(')'); }
        else if (m_tok == '!') { next(); node = Ast(Ast::Type::Not).add(expr(NOP)); }
        else {
            printf("bad expression\n");
            exit(-1);
        }
        while (m_tok >= level) {
            if      (m_tok == ADD) { next(); node = Ast(Ast::Add).add(std::move(node)).add(expr(MUL)); }
            else if (m_tok == SUB) { next(); node = Ast(Ast::Sub).add(std::move(node)).add(expr(MUL)); }
            else if (m_tok == MUL) { next(); node = Ast(Ast::Mul).add(std::move(node)).add(expr(NOP)); }
            else if (m_tok == DIV) { next(); node = Ast(Ast::Div).add(std::move(node)).add(expr(NOP)); }
            else if (m_tok == MOD) { next(); node = Ast(Ast::Mod).add(std::move(node)).add(expr(NOP)); }
            else {
                printf("bad expression\n");
                exit(-1);
            }
        }
        return node;
    }
    void match(int tok) {
        if (tok != m_tok) {
            printf("unexpected token\n");
            exit(-1);
        }
        next();
    }
};


int main(int argc, char** argv) {
    std::array<char, 256> line;
    for (;;) {
        printf("  ");
        if (!fgets(line.data(), line.size(), stdin)) break;
        Parser pars(line.data());
        pars.expr().print();
        printf("\n");
        pars.match(0);
    }
    return 0;
}
