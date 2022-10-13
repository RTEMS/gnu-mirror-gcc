/* { dg-do run } */
/* { dg-shouldfail-purecap "morello bounds" } */

struct large_struct { char x[99999]; };

static __attribute__ ((noinline)) char
goo (struct large_struct *a)
{
  /* The size of `large_struct` requires padding and alignment to ensure
     precise bounds (i.e. that it can't overlap with other variables).
     Hence we have to access x[1] rather than x[0] to ensure triggering the
     problem.  */
  return (a+1)->x[1];
}

__attribute__ ((noinline)) char
foo (struct large_struct arg)
{
  return goo (&arg);
}

int
main ()
{
  return foo ((struct large_struct){0});
}


