/* { dg-do run } */
/* { dg-skip-if "" { *-*-* } { "-mfake-capability" } { "" } }  */

__attribute ((noipa))
void * __capability foo (void * __capability t)
{
  asm("mov x20, #0" ::: "x20");
  return t;
}

int main(void)
{
  void * __capability c = __builtin_cheri_global_data_get ();
  if (!__builtin_cheri_equal_exact (c, foo (c)))
    __builtin_abort ();
}
