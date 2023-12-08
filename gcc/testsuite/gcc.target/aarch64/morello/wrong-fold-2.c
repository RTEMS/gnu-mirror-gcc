/* { dg-do run } */
/* { dg-require-effective-target cheri_capability_pure } */

/* This was ICEing at expand time due to a wrong match.pd transformation.

   This test further adds an execution test that should fail if the
   transformation were to fire.  */

char *q;
void f() {}

__attribute__((noipa))
void g()
{
  char *p = __builtin_cheri_offset_set(0, 2);
  q = p + (long)f - (long)p;
  q = __builtin_cheri_address_set(q, 0);
}

int main(void)
{
  g();
  if (!__builtin_cheri_equal_exact (q, (char *)0))
    __builtin_abort ();
}
