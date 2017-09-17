
int f;
int g;
int i;


void main() {
   f = 5;
   g = f() * f() * g();
   i = 10 + 3 * 4 + g();
   i = 2;
}


int f() {
   f = f + 1;
   return f;
}

void g() {}
