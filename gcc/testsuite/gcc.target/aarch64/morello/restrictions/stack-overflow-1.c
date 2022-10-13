/* Taken from ASAN testsuite.  */
/* { dg-do run } */
/* { dg-options "-fno-builtin-memset" } */
/* { dg-shouldfail-purecap "morello bounds" } */

extern
#ifdef __cplusplus
"C"
#endif
void *memset (void *, int, __SIZE_TYPE__);

volatile int ten = 10;

int main() {
  char x[10];
  memset(x, 0, 10);
  int res = x[ten];  /* BOOOM */
  return res;
}
