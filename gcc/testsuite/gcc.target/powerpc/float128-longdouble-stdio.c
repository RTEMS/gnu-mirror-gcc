/* { dg-require-effective-target ppc_float128_hw } */
/* { dg-options "-mdejagnu-cpu=power9 -O2 -mlong-double-128 -Wno-psabi -mabi=ieeelongdouble" } */

/* Test if switching long double to IEEE 128-bit maps the printf and scanf
   function names correctly.  We explicitly turn off PC-relative support to
   make it simpler to compare the call without having a @notoc qualifier.  */

#include <stdlib.h>

volatile long double x = 1.0L;
volatile long double y, z;

int
main (void)
{
  char buffer[100];

  /* { dg-final { scan-assembler {\m__sprintfieee128\(@notoc\)?\M} } }  */
  __builtin_sprintf (buffer, "%Lg", x);

  /* { dg-final { scan-assembler {\m__printfieee128\(@notoc\)?\M} } }  */
  __builtin_printf ("x is %Lg [%s]\n", x, buffer);

  /* { dg-final { scan-assembler {\m__isoc99_sscanfieee128\(@notoc\)?\M} } }  */
  __builtin_sscanf (buffer, "%Lg", &y);

  __builtin_printf ("Type 1.0: ");

  /* { dg-final { scan-assembler {\m__isoc99_scanfieee128\(@notoc\)?\M} } }  */
  __builtin_scanf ("%Lg", &z);

  if (x != y || x != z)
    abort ();

  return 0;
}
