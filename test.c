int i;
int s;

enum {
   CONSTANT = 42,
};

//int[L] a;

int main() {
   i = 10 * 4 + CONSTANT;

   for (i = CONSTANT; i; i = i - 1) s = s + i;
   return s;
}
