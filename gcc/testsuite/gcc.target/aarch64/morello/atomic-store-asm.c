/* { dg-do assemble } */
/* { dg-additional-options "-O2 -std=c99" } */
extern struct a { int b; } d[];
int *e;
long f;
_Bool g;
void k() {
  enum { ad } a;
  long b = f;
  struct a c = d[b];
ah:
  a = ad;
  int i = c.b;
  char j[i];
  if (g)
    goto am;
  for (int h = h;;)
    if (j[h])
      __atomic_store_n(&e, 0, 0);
am:
  if (a)
    goto ah;
}
