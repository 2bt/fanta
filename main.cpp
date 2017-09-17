// vim: et ts=4 sts=4 sw=4
#include <cstdio>
#include <array>
#include <vector>


enum Opcode {
    IMM, LI, SI, PUSH,
    JMP, JZ, JNZ, CALL, RET,
    INC, DEC, NOT, NEG,
    OR, XOR, AND, EQ, NE, LT, LE, GT, GE, SHL, SHR, ADD, SUB, MUL, DIV, MOD,
};



void run(const std::vector<int>& code, int pc) {
    std::array<int, 1024> mem = {};
    int sp = mem.size();
    mem[--sp] = code.size();

    int ax = 0;

    while (pc < (int) code.size()) {
        switch (code[pc++]) {
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
        default:    exit(1);
        }
    }

    for (int i = 0; i < 16; ++i) printf(" %d", mem[i]);
    printf("\n");
}


int main(int argc, char** argv) {

    FILE* f = fopen("bytecode", "r");
    if (!f) return 1;
    fseek(f, 0, SEEK_END);
    int size = ftell(f);
    rewind(f);
    std::vector<int> bytecode(size / sizeof(int));
    fread(bytecode.data(), sizeof(int), bytecode.size(), f);
    fclose(f);

    run(bytecode, 0);

    return 0;
}
