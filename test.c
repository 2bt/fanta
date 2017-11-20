enum { CONSTANT = 42 };
int i;
int s;

//int[L] a;

int main() {
   for (;;) i = i + 1;
   i = 10 * 4 + CONSTANT;
   for (i = CONSTANT; i; i = i - 1) s = s + i;
   return s;
}
