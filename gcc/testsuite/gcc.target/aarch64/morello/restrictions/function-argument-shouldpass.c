/* { dg-do run } */

/* This testcase ensures that our SCBNDSE on the large structure did not clear
   the tag.  This is not checked elsewhere in the testsuite.  */

struct large_struct { char x[99999]; };

static __attribute__ ((noinline)) char
goo (struct large_struct *a)
{
  return a->x[99998];
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



