/* { dg-do run } */
/* { dg-options "-fgimple -O2 -fipa-pta -fdump-ipa-pta2 -fipa-dlo" } */

#include <stdlib.h>
struct S { char* f0; struct S* f1;};

int __GIMPLE (startwith("ipa-pta")) main(int argc, char** argv)
{
struct S * p0;
int retval;
int _1;
int _2;

_1 = (long unsigned int) argc;
_2 = _1 * 16;
p0 = malloc (_2);
retval = 0;
return retval;
}

/* { dg-final { scan-ipa-dump "rule 1: p0_6 = malloc ._2." "pta2" } } */
