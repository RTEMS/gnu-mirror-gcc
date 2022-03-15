/* { dg-do run } */

/* This function previously caused aarch64_anchor_offset to emit a large
   sub followed by a large indexed load, which means we end up
   dereferencing an invalid capability.  */
__attribute__((noipa))
int *f(int **p) {
  return p[-17];
}

int *arr[18];
int main(void) {
  int x = 42;
  arr[0] = &x;
  if (*f(arr + 17) != 42)
    __builtin_abort ();
  return 0;
}
