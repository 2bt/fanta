#include "parser.hpp"
#include <cstdio>




int main(int argc, char** argv) {

    Parser parser(
R"(


int* a;

    )");

    RootNode* s = parser.parse_program();
    s->print();
    delete s;

    return 0;
}
