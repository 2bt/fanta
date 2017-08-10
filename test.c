
int a = 5;

int foo() {
   a = a + 5;
   return a * 2;
}

int main() {
   a = a * 3;
   return foo();
}
