#include "parser.hpp"
#include <cstdio>




int main(int argc, char** argv) {

    Parser parser
(R"(

enum {
    A,
    B,
    C = A + 4 * (10 << 1),
    D,
};
enum {
    FOO = 42
};


struct Vec {
    int x;
    int y;
};

struct Bar {
    Vec  pos;
    int* pointers[100];
};


int* a;


int foo(Vec* v) {
    return v->x * v->y;
}


int main() {


    return 0;
}

)");

    RootNode* s = parser.parse_program();
    s->print();
    delete s;

    return 0;
}
