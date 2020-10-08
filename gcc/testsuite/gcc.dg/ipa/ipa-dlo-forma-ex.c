/* { dg-do link } */
/* { dg-options "-w -O -fipa-pta -fdump-ipa-pta2 -fipa-dlo -fgimple -fno-dce -fno-dse -fno-inline -fno-tree-forwprop" } */

#include <stdlib.h>

struct A { char *f1; struct A *f2;};

int __GIMPLE(startwith("ipa-pta"))
main (int argc, char * * argv)
{
  struct A * p2;
  struct A * p1;
  char * pc;
  char buffer[100];
  int j;
  int i;
  int _1;
  int _2;
  long unsigned int _3;
  long unsigned int _4;
  long unsigned int _5;
  long unsigned int _6;
  long unsigned int _7;
  __SIZETYPE__ _8;
  __SIZETYPE__ _9;
  long unsigned int s;
  struct A * _10;
  struct A * _11;
  int _27;

  i_15 = argc_14(D)  -1;
  _1 = rand ();
  _2 = _1 % i_15;
  j_18 = i_15 - _2;
  pc = malloc(_1);
  _3 = (long unsigned int) argc_14(D);
  s = (long unsigned int) 16;
  _4 = _3 * s;
  p1 = malloc (_4);
  _5 = (long unsigned int) j_18;
  _6 = _5 * s;
  p2 = p1 + _6;
  p2->f1 = pc;
  _7 = (long unsigned int) i_15;
  _8 = _7 * s;
  _9 = _8 - s;
  _10 = p1 + _9;
  _11 = p1 + _8;
  _10->f2 = _11;
  _27 = (int) 0;
  return _27;
}

/* { dg-final { scan-ipa-dump "pc_31 = { pc_31 }" "pta2" } } */
/* { dg-final { scan-ipa-dump "_11_38 = { p1_34 p2_35 _10_37 _11_38 }" "pta2" } } */
/* { dg-final { scan-ipa-dump "0 .0.	<- 0\n	-> 0 1" "pta2" } } */
/* { dg-final { scan-ipa-dump "1 .0.	<- 0\n	->" "pta2" } } */
