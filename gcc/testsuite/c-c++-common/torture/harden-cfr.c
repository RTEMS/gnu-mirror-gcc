/* { dg-do run } */
/* { dg-options "-fharden-control-flow-redundancy -fdump-tree-hardcfr --param hardcfr-max-blocks=9 --param hardcfr-max-inline-blocks=5 -ffat-lto-objects" } */

int
f (int i, int j)
{
  if (i < j)
    return 2 * i;
  else
    return 3 * j;
}

int
g (unsigned i, int j)
{
  switch (i)
    {
    case 0:
      return j * 2;

    case 1:
      return j * 3;

    case 2:
      return j * 5;

    default:
      return j * 7;
    }
}

int
h (unsigned i, int j) /* { dg-warning "has more than 9 blocks, the requested maximum" } */
{
  switch (i)
    {
    case 0:
      return j * 2;

    case 1:
      return j * 3;

    case 2:
      return j * 5;

    case 3:
      return j * 7;

    case 4:
      return j * 11;

    case 5:
      return j * 13;

    case 6:
      return j * 17;

    case 7:
      return j * 19;

    default:
      return j * 23;
    }
}

int
main ()
{
  if (f (1, 2) != 2 || f (3, 2) != 6
      || g (2, 5) != 25 || h (4, 3) != 33)
    __builtin_abort ();
  /* Call exit, instead of returning, to avoid an edge to the exit block and
     thus implicitly disable hardening of main.  */
  __builtin_exit (0);
}

/* Inlined checking thus trap for f.  */
/* { dg-final { scan-tree-dump-times "__builtin_trap" 1 "hardcfr" } } */
/* Out-of-line checking for g.  */
/* { dg-final { scan-tree-dump-times "__hardcfr_check" 1 "hardcfr" } } */
/* No checking for h (too many blocks) or main (no edges to exit block).  */
