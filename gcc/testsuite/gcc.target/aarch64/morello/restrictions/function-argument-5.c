/* { dg-do run } */
/* { dg-shouldfail-purecap "morello bounds" } */

/* When this is passed on the stack, we round the size of the stack slot that
   we generate up to a stack alignment of 16.  This is done in
   assign_stack_temp_for_type.  That rounding up includes the bounds of the
   pointer, and seems to require applying to the bounds of the pointer used
   since later uses of the same stack slot may require the entire space
   allocated.
   It is for this reason that we allocate a structure of 112 bytes (since that
   is divisible by 16 which is the size to which we round stack slots up to --
   hence accessing one past the structure will cause a problem).
   MORELLO TODO It would be nice to look into this behaviour and see how
   feasible it is to ensure that these stack slots can work without such a
   rounding up.   */
struct large_struct { char x[112]; };

static __attribute__ ((noinline)) char
goo (struct large_struct *a)
{
  return (a+1)->x[0];
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

