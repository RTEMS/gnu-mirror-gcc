/* { dg-do compile { target { powerpc*-linux-* } } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-O2 -mvsx -mabi=ibmlongdouble -Wno-psabi" } */
/* { dg-final { scan-assembler "gnu_attribute 4, 5" {xfail *-*-*} } } */

/* Check that if we can do the long double operation without doing an emulator
   call, such as with copysign, that we still set the appropriate
   .gnu_attribute.

   However, the code that did this in rs6000_emit_move has been removed because
   it could not differentiate between long double and another type that uses
   the same mode.  This test is marked as xfail until a gimple pass is added to
   track the use of long double types.  */

long double a, b, c;

void cs (void)
{
  a = __builtin_copysignl (b, c);
}
