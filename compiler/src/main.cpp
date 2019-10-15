#include "parser.hpp"
#include <cstdio>




int main(int argc, char** argv) {

    Parser parser
(R"(

enum { A, B, C };

struct Vec;

struct Vec {
    int x;
    int y;
};

struct Bar {
    Vec  pos;
    int* pointers[100];
};


int foo(Vec* v);

int foo(Vec* v) {
    a = 1 + C;
    return v->x * v->y;
}

Vec v;
int i;

int main() {
    v.x = 1;
    v.y = 3;
    i = foo(&v);


    return 0;
}

)");

    RootNode* s = parser.parse_program();
    s->print();
    delete s;

    return 0;
}
