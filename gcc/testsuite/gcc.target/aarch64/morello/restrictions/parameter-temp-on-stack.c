/* { dg-do run } */
/* { dg-shouldfail-purecap "morello bounds" } */

struct D { __uintcap_t x; unsigned long y; };
void abort ();
extern void takes_struct_addr (struct D *y);
int ret;

void __attribute__ ((noinline, noclone))
takes_struct_addr (struct D *val)
{
  /* N.b. we test with a read since the write can overwrite the return pointer
     on the stack in an optimised compilation and crash even though we don't
     have stack bounds turned on.  */
  ret = (val+1)->x; // BOOM
}


void __attribute__ ((noinline))
f1 (struct D x)
{
  takes_struct_addr (&x);
}

int main()
{
  struct D basic_arg = { 100, 10 };
  f1(basic_arg);
  return 0;
}
