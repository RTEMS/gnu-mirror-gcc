/* { dg-do compile } */
/* { dg-options "-fharden-control-flow-redundancy -fdump-tree-hardcfr -ffat-lto-objects" } */

int f(int i) {
  if (i)
    __builtin_return (&i);
  return i;
}

/* Out-of-line checking, before both builtin_return and return.  */
/* { dg-final { scan-tree-dump-times "__hardcfr_check" 2 "hardcfr" } } */
