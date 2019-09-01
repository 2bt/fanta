#include "parser.hpp"
#include <cstdio>




int main(int argc, char** argv) {

    Parser parser(
R"({
    # test
    a[0].hallo = 3;
    q = a[2].hallo.foo[3 + 4] * 3 - func().a % 10;

    if (a == 1) {
        foo();
    }
    else b = 1;
})");

    Node* s = parser.stmt();
    s->print();
    delete s;

    return 0;
}
