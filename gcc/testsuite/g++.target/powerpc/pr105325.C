/* { dg-do assemble } */
/* { dg-require-effective-target lp64 } */
/* { dg-require-effective-target powerpc_prefixed_addr } */
/* { dg-options "-O2 -mdejagnu-cpu=power10 -fstack-protector" } */

/* Test that power10 fusion does not generate an LWA/CMPDI instruction pair
   instead of PLWZ/CMPWI.  Ultimately the code was dying because the fusion
   load + compare -1/0/1 patterns did not handle the possibility that the load
   might be prefixed.  */

struct Ath__array1D {
  int _current;
  int getCnt() { return _current; }
};
struct extMeasure {
  int _mapTable[10000];
  Ath__array1D _metRCTable;
};
void measureRC() {
  extMeasure m;
  for (; m._metRCTable.getCnt();)
    for (;;)
      ;
}
