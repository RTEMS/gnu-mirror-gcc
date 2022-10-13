/* Taken from ASAN testsuite.  */
/* { dg-do run } */
/* { dg-options "-fno-builtin-malloc -fno-builtin-free -fno-builtin-memset" } */
/* { dg-shouldfail-purecap "morello bounds" } */

#ifdef __cplusplus
extern "C" {
#endif

void *memset (void *, int, __SIZE_TYPE__);
void *malloc (__SIZE_TYPE__);
void free (void *);

#ifdef __cplusplus
}
#endif

volatile int ten = 10;
int main(int argc, char **argv) {
  char *x = (char*)malloc(10);
  memset(x, 0, 10);
  int res = x[ten];  /* BOOOM */
  x[ten] = res%3;
  free(x);
  return 0;
}
