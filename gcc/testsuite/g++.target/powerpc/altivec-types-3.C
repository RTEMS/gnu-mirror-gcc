/* { dg-do assemble } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-O -fstack-protector-all" } */
/* { dg-require-effective-target powerpc_prefixed_addr -mdejagnu-cpu=power10 } */

// If -fstack-protector-all is on, the load fusion code would generate a 'lwa'
// instead of a 'plwz' instruction with a large offset.

#ifndef NUM
#define NUM 10000
#endif

struct Ath__array1D {
  int _current;
  int getCnt() { return _current; }
};
struct extMeasure {
  int _mapTable[NUM];
  Ath__array1D _metRCTable;
};
void measureRC() {
  extMeasure m;
  for (; m._metRCTable.getCnt();)
    for (;;)
      ;
}
