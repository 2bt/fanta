enum { CONSTANT = 42 };
int i;
int s;

int main() {
   for (i = 10 * 4 + CONSTANT; i; i = i - 1) s = s + i;
   return s;
}
