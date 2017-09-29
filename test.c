int i;
int s;

enum {
   CONSTANT = 42,
};

//int[L] a;

int main() {
   for (i = CONSTANT; i; i = i - 1) s = s + i;
   return s;
}
