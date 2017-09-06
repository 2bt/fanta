// vim: et ts=4 sts=4 sw=4
// a simple math expression evaluator to teach me TDOP parsing
#include <cstdio>
#include <cmath>
#include <cstring>
#include <array>
#include <vector>
#include <string>
#include <err.h>


class Lexer {
protected:
    enum Token {
        NUM = 128, ID,

        // operators in precedence order
        ASGN, OR, XOR, AND, // TODO
        EQU, NEQ, LTN, GTN, LEQ, GEQ, SHL, SHR, ADD, SUB, MUL, DIV, MOD, NOP
    };
    void init(const char* pos) {
        m_pos = pos;
        next();
    }
    void next() { m_tok = scan(); }

    int         m_tok;
    int         m_num;
    std::string m_name;

private:
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

            if (isalpha(c) || c == '_') {
                m_name = c;
                while (isalnum(*m_pos) || *m_pos == '_') m_name += *m_pos++;
                return ID;
            }

            if (strchr("(){};~", c)) return c;
            if (!isspace(c)) {
                printf("unexpected character\n");
                exit(-1);
            }
        }
        return 0;
    }

    const char* m_pos;
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

        Var,
        Call,
        Asgn,
        Ret,
        Block,
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
        printf("(%c", "?#+-_!*/%$C=R{"[type]);
        if (type == Var) printf("%s", name.c_str());
        if (type == Num) printf("%d", number);
        for (const Ast& a : kids) a.print();
        printf(")");
    }

};


struct Function {
    std::string name;
    Ast         ast;
};


struct Program {
    std::vector<Function> functions;
    void print() const {
        for (const Function& f : functions) {
            printf("%s ", f.name.c_str());
            f.ast.print();
            printf("\n");
        }
    }
};



class Parser : public Lexer {
public:
    Program parse(char* pos) {
        init(pos);

        Program program;

        while (m_tok != 0) {
            if (m_tok == ID) {

                Function func;
                func.name = m_name;

                next();
                match('(');
                match(')');
                if (m_tok != '{') errx(1, "no function block");
                func.ast = stmt();

                program.functions.emplace_back(std::move(func));
                continue;
            }
            errx(1, "error parsing program");
        }
        return program;
    }

private:
    Ast stmt() {
        if (m_tok == -1) {
            // ...
            return Ast(-1);
        }
        else if (m_tok == '{') {
            next();
            Ast node(Ast::Block);
            while (m_tok != '}') node.add(stmt());
            next();
            return node;
        }
        else {
            Ast node = expr(ASGN);
            match(';');
            return node;
        }
    }

    Ast expr(int level = EQU) {
        Ast node;

        // unary stuff
        if      (m_tok == NUM) { next(); node = Ast(m_num); }
        else if (m_tok == ADD) { next(); node = expr(NOP); }
        else if (m_tok == SUB) { next(); node = Ast(Ast::Type::Neg).add(expr(NOP)); }
        else if (m_tok == '(') { next(); node = expr(); match(')'); }
        else if (m_tok == '!') { next(); node = Ast(Ast::Type::Not).add(expr(NOP)); }
        else if (m_tok == ID) {
            next();
            if (m_tok == '(') {
                errx(1, "TODO: function call");
            }
            node = Ast(Ast::Type::Var);
            node.name = m_name;
        }
        else errx(1, "bad expression %d", m_tok);

        // binary and postfix operators
        while (m_tok >= level) {
            if      (m_tok == ASGN) { next(); node = Ast(Ast::Asgn).add(std::move(node)).add(expr(ASGN)); }
            else if (m_tok == ADD) { next(); node = Ast(Ast::Add).add(std::move(node)).add(expr(MUL)); }
            else if (m_tok == SUB) { next(); node = Ast(Ast::Sub).add(std::move(node)).add(expr(MUL)); }
            else if (m_tok == MUL) { next(); node = Ast(Ast::Mul).add(std::move(node)).add(expr(NOP)); }
            else if (m_tok == DIV) { next(); node = Ast(Ast::Div).add(std::move(node)).add(expr(NOP)); }
            else if (m_tok == MOD) { next(); node = Ast(Ast::Mod).add(std::move(node)).add(expr(NOP)); }
            else errx(1, "bad expression %d", m_tok);
        }
        return node;
    }
    void match(int tok) {
        if (tok != m_tok) errx(1, "unexpected token %d (should be %d)", tok, m_tok);
        next();
    }
};


enum Opcode {
    IMM, LI, SI, PUSH,
    JMP, JZ, JNZ, CALL, RET,
    INC, DEC, NOT, NEG,
    OR, XOR, AND, EQ, NE, LT, LE, GT, GE, SHL, SHR, ADD, SUB, MUL, DIV, MOD,
    PRINT
};


void run(const std::vector<int>& code) {
    std::array<int, 1024> mem;
    int sp = mem.size();

    int pc = 0;
    int ax = 0;

    while (pc < (int) code.size()) {
        switch (int op = code[pc++]) {
        case IMM:   ax = code.at(pc++); break;
        case LI:    ax = mem.at(ax); break;
        case SI:    mem.at(mem.at(sp++)) = ax; break;
        case PUSH:  mem.at(--sp) = ax; break;
        case JMP:   pc = code.at(pc); break;
        case JZ:    pc = ax ? pc + 1 : code.at(pc); break;
        case JNZ:   pc = ax ? code.at(pc) : pc + 1; break;
        case CALL:  mem.at(--sp) = pc + 1; pc = code.at(pc); break;
        case RET:   pc = mem.at(sp++); break;
        case INC:   ++ax; break;
        case DEC:   --ax; break;
        case NOT:   ax = !ax; break;
        case NEG:   ax = -ax; break;
        case OR:    ax = mem.at(sp++) | ax; break;
        case XOR:   ax = mem.at(sp++) ^ ax; break;
        case AND:   ax = mem.at(sp++) & ax; break;
        case EQ:    ax = mem.at(sp++) == ax; break;
        case NE:    ax = mem.at(sp++) != ax; break;
        case LT:    ax = mem.at(sp++) < ax; break;
        case LE:    ax = mem.at(sp++) <= ax; break;
        case GT:    ax = mem.at(sp++) > ax; break;
        case GE:    ax = mem.at(sp++) >= ax; break;
        case SHL:   ax = mem.at(sp++) << ax; break;
        case SHR:   ax = mem.at(sp++) >> ax; break;
        case ADD:   ax = mem.at(sp++) + ax; break;
        case SUB:   ax = mem.at(sp++) - ax; break;
        case MUL:   ax = mem.at(sp++) * ax; break;
        case DIV:   ax = mem.at(sp++) / ax; break;
        case MOD:   ax = mem.at(sp++) % ax; break;
        case PRINT: printf("%d\n", ax); break;
        default:    errx(1, "unknown instruction: %d", op); break;
        }
    }

    for ( int i = 0; i < 10; ++i ) printf(" %d", mem[i]);
    printf("\n");
}


int main(int argc, char** argv) {
/*
    std::vector<int> code = {
        IMM, 2, PUSH, IMM, 10, SI,
        PRINT,
        IMM, 2, PUSH, LI, DEC, SI,
        JNZ, 6
    };
    run(code);
*/

    FILE* f = fopen("test.c", "r");
    if (!f) return 1;
    fseek(f, 0, SEEK_END);
    int size = ftell(f);
    rewind(f);
    std::vector<char> bytes(size + 1);
    fread(bytes.data(), 1, size, f);
    fclose(f);

    Program program = Parser().parse(bytes.data());
    program.print();

    return 0;
}
