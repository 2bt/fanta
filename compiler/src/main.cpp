#include "parser.hpp"
#include <cstdio>




int main(int argc, char** argv) {

    Parser parser(R"(
        a[2].hallo * 3 - func()
    )");

    Node* s = parser.expr();
    s->print();
    delete s;

    return 0;
}
