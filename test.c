int a;
int f;
int g;
int i;


void main() {
   a = 1;
   f = 5;
   foo();
   g = f() * g();
   i = 10 + 3 * 4;
}


int f() {
   f = !f;
   return 10;
}

int g() {
   return 1;
}

void foo() {
   if (a == 0) a = 10;
   else a = 3 * 3;
}
