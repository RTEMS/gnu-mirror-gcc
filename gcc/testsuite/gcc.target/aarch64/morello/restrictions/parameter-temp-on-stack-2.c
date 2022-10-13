/* { dg-do run } */
/* { dg-shouldfail-purecap "morello bounds" } */

void abort ();
extern void takes_int_addr (int *y);

void __attribute__ ((noinline, noclone))
takes_int_addr (int *x)
{
  /* N.b. we test with a read since the write can overwrite the return pointer
     on the stack in an optimised compilation and crash even though we don't
     have stack bounds turned on.  */
  if (x[1] == 100) // BOOM
    abort();
}


void __attribute__ ((noinline))
f1 (int x)
{
  takes_int_addr (&x);
}

int main()
{
  f1(20);
  return 0;
}
