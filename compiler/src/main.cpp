#include "lexer.hpp"
#include <cstdio>


int main(int argc, char** argv) {

    Lexer lex(R"(

        int a = 0;
        a = 5 + a * 3;

    )");

    for (;;) {
        Token t = lex.next_token();
        printf("token %d, '%c', %s, %d\n", t.type, t.type, t.name.c_str(), t.number);

        if (t.type == T_EOF) break;
    }

    return 0;
}
