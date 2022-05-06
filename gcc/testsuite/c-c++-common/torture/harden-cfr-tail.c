/* { dg-do compile } */
/* { dg-options "-fharden-control-flow-redundancy -fdump-tree-hardcfr -ffat-lto-objects" } */

/* We'd like to check that we insert checking so as to not disrupt tail calls,
   but unfortunately mandatory tail calls are not available in C, and optimizing
   calls as tail calls only takes place after hardcfr.  */

extern int g(int i);

int f(int i) {
  return g (i);
}

/* Inline checking before the tail call.  */
/* { dg-final { scan-tree-dump-times "__builtin_trap" 1 "hardcfr" } } */
/* Inline checking before the tail call.  */
/* { dg-final { scan-tree-dump-times "\\\[tail\]" 1 "hardcfr" { xfail *-*-* } } } */
