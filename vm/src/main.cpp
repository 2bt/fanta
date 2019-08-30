#include <cstdio>
#include <cassert>
#include <array>
#include <vector>


enum {
    MEM_SIZE = 2 << 16,
};

enum {
    OP_EXIT,
    OP_IMM,
    OP_LD,
    OP_ST,
    OP_PUSH,
    OP_JMP,
    OP_JZ,
    OP_JNZ,
    OP_CALL,
    OP_ENT,
    OP_ADJ,
    OP_LEV,
    OP_LEA,

    OP_OR,
    OP_AND,
    OP_EQ,
    OP_NE,
    OP_LT,
    OP_LE,
    OP_GT,
    OP_GE,
    OP_ADD,
    OP_SUB,
    OP_MUL,
    OP_DIV,
    OP_MOD,
};

struct VM {
    int cycle = 0;
    int ax    = 0;
    int bp    = 0;
    int sp    = MEM_SIZE;
    int pc    = 0;
    std::array<int, MEM_SIZE> mem = {};
    int const*                code;

    int  next() {
        return code[pc++];
    }
    int  get_mem(int x) const {
        assert(x > 0 && x < mem.size());
        return mem[x];
    }
    void set_mem(int x, int v) {
        assert(x > 0 && x < mem.size());
        mem[x] = v;
    }
    void push(int v) {
        --sp;
        assert(sp > 0 && sp < mem.size());
        mem[sp] = v;
    }
    int  pop() {
        assert(sp > 0 && sp < mem.size());
        return mem[sp++];
    }


    int clock() {
        ++cycle;
        int op = next();
        printf("op = %d, ax = %d, mem =", op, ax);
        for (int i = 0; i < 8; ++i) printf(" %d", mem[i]);
        printf("\n");

        switch (op) {
        case OP_EXIT: break;
        case OP_IMM:  ax = next(); break;
        case OP_LD:   ax = get_mem(ax); break;
        case OP_ST:   set_mem(pop(), ax); break;
        case OP_PUSH: push(ax); break;
        case OP_JMP:  pc = next(); break;
        case OP_JZ:   pc = ax ? pc + 1 : next(); break;
        case OP_JNZ:  pc = ax ? next() : pc + 1; break;
        case OP_CALL: push(pc + 1); pc = next(); break;
        case OP_ENT:  push(bp), bp = sp; sp -= next(); break;
        case OP_ADJ:  sp += next(); break;
        case OP_LEV:  sp = bp; bp = pop(); pc = pop(); break;
        case OP_LEA:  ax = bp + next(); break;

        case OP_OR:   ax = pop() | ax; break;
        case OP_AND:  ax = pop() & ax; break;
        case OP_EQ:   ax = pop() == ax; break;
        case OP_NE:   ax = pop() != ax; break;
        case OP_LT:   ax = pop() < ax; break;
        case OP_LE:   ax = pop() <= ax; break;
        case OP_GT:   ax = pop() > ax; break;
        case OP_GE:   ax = pop() >= ax; break;

        case OP_ADD:  ax = pop() + ax; break;
        case OP_SUB:  ax = pop() - ax; break;
        case OP_MUL:  ax = pop() * ax; break;
        case OP_DIV:  ax = pop() / ax; break;
        case OP_MOD:  ax = pop() % ax; break;

        default: assert(0);
        }
        return op;
    }

    void exec() {
        while (clock() != OP_EXIT) {}
    }
};


const int code[] = {
    OP_IMM, 1,
    OP_PUSH,
        OP_IMM, 42,
        OP_PUSH,
        OP_IMM, 23,
        OP_ADD,
    OP_ST,

    OP_EXIT,
};


int main(int argc, char** argv) {

    VM vm;
    vm.code = code;
    vm.exec();

//    FILE* f = fopen("bytecode", "rb");
//    if (!f) return 1;
//    fseek(f, 0, SEEK_END);
//    int size = ftell(f);
//    rewind(f);
//    std::vector<int> code(size / sizeof(int));
//    fread(code.data(), sizeof(int), code.size(), f);
//    fclose(f);
//    VM vm;
//    vm.code = code.data();
//    vm.exec();

    return 0;
}
