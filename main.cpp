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
        RETURN, IF, ELSE, WHILE,


        // operators in precedence order
        ASGN, OR, XOR, AND, // TODO
        EQ, NE, LT, GT, LE, GE, SHL, SHR, ADD, SUB, MUL, DIV, MOD, NOP
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
            if (c == '/') {
                if (*m_pos != '/') return DIV;
                while (*m_pos && *m_pos != '\n') ++m_pos;
                continue;
            }
            if (c == '%') return MOD;
            if (c == '<') return *m_pos == '<' ? ++m_pos, SHL :
                                 *m_pos == '=' ? ++m_pos, LE : LT;
            if (c == '>') return *m_pos == '>' ? ++m_pos, SHR :
                                 *m_pos == '=' ? ++m_pos, GE : GT;
            if (c == '!') return *m_pos == '=' ? ++m_pos, NE : c;
            if (c == '=') return *m_pos == '=' ? ++m_pos, EQ : ASGN;

            if (isalpha(c) || c == '_') {
                m_name = c;
                while (isalnum(*m_pos) || *m_pos == '_') m_name += *m_pos++;
                if (m_name == "return") return RETURN;
                if (m_name == "if")     return IF;
                if (m_name == "else")   return ELSE;
                if (m_name == "while")  return WHILE;
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
        Num,
        Not, Neg,
        Or, Xor, And, Eq, Ne, Lt, Le, Gt, Ge, Shl, Shr, Add, Sub, Mul, Div, Mod,
        Var,
        Asgn,
        Block,

        Call,
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
};


struct Function {
    std::string name;
    int         address = -1;
    Ast         block;
};


struct Program {
    std::vector<Function> functions;
};



class Parser : public Lexer {
public:
    Program parse(char* pos) {
        init(pos);

        Program prog;

        while (m_tok != 0) {
            if (m_tok == ID) {

                Function func;
                func.name = m_name;

                next();
                match('(');
                match(')');
                if (m_tok != '{') errx(1, "no function block");
                func.block = stmt();

                prog.functions.emplace_back(std::move(func));
                continue;
            }
            errx(1, "error parsing prog");
        }
        return prog;
    }

private:
    Ast stmt() {
        if (m_tok == RETURN) {
            next();
            Ast node(Ast::Ret);
            if (m_tok != ';') node.add(expr());
            match(';');
            return node;
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

    Ast expr(int level = EQ) {
        Ast node;

        // unary stuff
        if      (m_tok == NUM) { next(); node = Ast(m_num); }
        else if (m_tok == ADD) { next(); node = expr(NOP); }
        else if (m_tok == SUB) { next(); node = Ast(Ast::Neg).add(expr(NOP)); }
        else if (m_tok == '(') { next(); node = expr(); match(')'); }
        else if (m_tok == '!') { next(); node = Ast(Ast::Not).add(expr(NOP)); }
        else if (m_tok == ID) {
            next();
            if (m_tok == '(') {
                node = Ast(Ast::Call);
                node.name = m_name;
                next();
                match(')');
            }
            else {
                node = Ast(Ast::Var);
                node.name = m_name;
            }
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
};


class Compiler {
public:

    std::vector<int>& code() { return m_code; }

    void compile(Program& prog) {

        // TODO analyzsis

        m_program = &prog;
        m_variables.clear();
        m_code.clear();

        for (Function& func : prog.functions) {
            func.address = m_code.size();

            compile(func.block);

            m_code.emplace_back(RET);
        }

    }
private:

    void address(Ast& node) {
        switch (node.type) {
        case Ast::Var:
            int i;
            for (i = 0; i < (int) m_variables.size(); ++i) {
                if (node.name == m_variables[i]) break;
            }
            if (i == (int) m_variables.size()) m_variables.emplace_back(node.name);
            m_code.emplace_back(IMM);
            m_code.emplace_back(i);
            break;
        default:
            errx(1, "address %d", node.type);
        }
    }

    void compile(Ast& node) {
        switch (node.type) {
        case Ast::Block:
            for (Ast& n : node.kids) compile(n);
            return;

        case Ast::Asgn:
            address(node.kids[0]);
            m_code.emplace_back(PUSH);
            compile(node.kids[1]);
            m_code.emplace_back(SI);
            return;

        case Ast::Num:
            m_code.emplace_back(IMM);
            m_code.emplace_back(node.number);
            return;

        case Ast::Neg:
            compile(node.kids[0]);
            m_code.emplace_back(NEG);
            return;

        case Ast::Not:
            compile(node.kids[0]);
            m_code.emplace_back(NOT);
            return;

        // binary operators
        case Ast::Or:
        case Ast::Xor:
        case Ast::And:
        case Ast::Eq:
        case Ast::Ne:
        case Ast::Lt:
        case Ast::Le:
        case Ast::Gt:
        case Ast::Ge:
        case Ast::Shl:
        case Ast::Shr:
        case Ast::Add:
        case Ast::Sub:
        case Ast::Mul:
        case Ast::Div:
        case Ast::Mod:
            compile(node.kids[0]);
            m_code.emplace_back(PUSH);
            compile(node.kids[1]);
            m_code.emplace_back(node.type - Ast::Or + OR);
            return;

        case Ast::Var:
            int i;
            for (i = 0; i < (int) m_variables.size(); ++i) {
                if (node.name == m_variables[i]) break;
            }
            if (i == (int) m_variables.size()) m_variables.emplace_back(node.name);
            m_code.emplace_back(IMM);
            m_code.emplace_back(i);
            m_code.emplace_back(LI);
            return;

        case Ast::Call:
            m_code.emplace_back(CALL);
            for (Function& func : m_program->functions) {
                if (node.name == func.name) {
                    // TODO: we need another pass
                    if (func.address == -1) errx(1, "function address not know yet");
                    m_code.emplace_back(func.address);
                    return;
                }
                if (func.address == -1) errx(1, "unknown function '%s'", node.name.c_str());
            }
            m_code.emplace_back(0);
            return;


        case Ast::Ret:
            if (!node.kids.empty()) compile(node.kids[0]);
            m_code.emplace_back(RET);
            return;



        default:
            errx(1, "compile %d", node.type);
        }
    }

    std::vector<std::string> m_variables;
    std::vector<int>         m_code;
    Program*                 m_program;
};



void run(const std::vector<int>& code, int pc) {
    std::array<int, 1024> mem = {};
    int sp = mem.size();
    mem[--sp] = code.size();

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
        default:    errx(1, "unknown instruction: %d", op); break;
        }
    }

    for (int i = 0; i < 10; ++i) printf(" %d", mem[i]);
    printf("\n");
}


int main(int argc, char** argv) {

    FILE* f = fopen("test.c", "r");
    if (!f) return 1;
    fseek(f, 0, SEEK_END);
    int size = ftell(f);
    rewind(f);
    std::vector<char> bytes(size + 1);
    fread(bytes.data(), 1, size, f);
    fclose(f);

    Program prog = Parser().parse(bytes.data());

    Compiler comp;
    comp.compile(prog);

    for (int i : comp.code()) printf(" %d", i);
    printf("\n");


    int pc = 0;
    for (Function& func : prog.functions) {
        if (func.name == "main") {
            pc = func.address;
            break;
        }
    }
    run(comp.code(), pc);

    return 0;
}
