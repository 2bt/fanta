
int f;
int g;
int i;


void main() {
   f = 5;
   foo();
   g = f() * f() * g();
   i = 10 + 3 * 4;
}


int f() {
   f = f + 1;
   return f;
}

int g() {
   return 1;
}

void foo() {

}
