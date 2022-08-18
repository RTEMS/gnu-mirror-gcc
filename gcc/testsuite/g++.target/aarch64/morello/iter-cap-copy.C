/* { dg-do run } */
/* This was getting miscompiled since the C++ FE emitted a
   block copy for *p = t, then SRA split out an underaligned capability
   store for this, and expand split that store into pairs of x-register
   loads/stores, which invalidated the capability.

   Instead, we now use memcpy when expanding the underaligned capability
   store.  */

struct iter {
  unsigned long *p;
  int y;
  iter() {}
};

__attribute__((noipa))
void f(iter *p, unsigned long *q)
{
  iter t;
  t.p = q;
  *p = t;
}

int main()
{
  iter i;
  unsigned long x = 0xdeadbeefUL;
  f(&i, &x);
  if (*i.p != 0xdeadbeefUL)
    __builtin_abort ();
}
