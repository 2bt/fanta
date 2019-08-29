#include "lexer.hpp"
#include <cstdio>


int main(int argc, char** argv) {

    Lexer lex(R"(

        int a = 0;
        a = 5 + a * 3;

    )");

    Token t = lex.next_token();

    return 0;
}
