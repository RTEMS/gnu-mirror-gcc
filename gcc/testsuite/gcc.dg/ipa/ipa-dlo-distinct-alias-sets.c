/* { dg-do link } */
/* { dg-options "-fgimple -O -fipa-pta -fdump-ipa-pta -fipa-dlo" } */

#include <stdlib.h>
struct S { char* f0; struct S* f1;};

int __GIMPLE (startwith("ipa-pta")) main(int argc, char** argv)
{
struct S * p0;
struct S * p1;
int retval;
int _1;
int _2;

_1 = (long unsigned int) argc;
_2 = _1 * 16;
p0 = malloc (_2);
p1 = malloc (_2);
retval = 0;
return retval;
}

/* { dg-final { scan-ipa-dump "p1_8 = { p1_8 }" "pta2" } } */
/* { dg-final { scan-ipa-dump "p0_6 = { p0_6 }" "pta2" } } */
