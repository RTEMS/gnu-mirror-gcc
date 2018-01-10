/* { dg-do compile { target { powerpc*-linux-* && lp64 } } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-O2 -mpower9-vector -mabi=ieeelongdouble -Wno-psabi" } */
/* { dg-final { scan-assembler "gnu_attribute 4, 13" } } */

/* Check that if we can do the long double operation without doing an emulator
   call, such as with IEEE 128-bit hardware support on power9, that we still
   set the appropriate .gnu_attribute.  */

long double a;

void add1 (void)
{
  a++;
}
