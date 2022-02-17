/* { dg-do compile } */
/* { dg-options "-fharden-control-flow-redundancy -fdump-tree-hardcfr -ffat-lto-objects" } */

int f(int i) {
  if (!i)
    __builtin_abort ();
  return i;
}

/* No checking before the noreturn abort, so single inline check.  */
/* { dg-final { scan-tree-dump-times "__builtin_trap" 1 "hardcfr" } } */
