
int f;
int g;
int i;


int f() {
   f = f + 1;
   return f;
}

void main() {
   f = 5;
   g = f() * f() * f();
   i = 10 + 3 * 4;
}


