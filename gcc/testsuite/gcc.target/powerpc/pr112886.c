/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -O2" } */

/* PR target/112886: Test that print_operand %S<n> gives the correct register
   number for VSX registers (i.e. if the register is an Altivec register, the
   register number is 32..63 instead of 0..31.  */

void
test (__vector_pair *ptr1, __vector_pair *ptr2, __vector_pair *ptr3)
{
  register __vector_pair p asm ("vs10");
  register __vector_pair q asm ("vs42");
  register __vector_pair r asm ("vs44");

  q = *ptr2;
  r = *ptr3;

  __asm__ ("xvadddp %x0,%x1,%x2\n\txvadddp %S0,%S1,%S2"
	   : "=wa" (p)
	   : "wa"  (q), "wa" (r));

  *ptr1 = p;
}

/* { dg-final { scan-assembler-times {\mxvadddp 10,42,44\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvadddp 11,43,45\M} 1 } } */
/* { dg-final { scan-assembler-times {\mlxvpx?\M}           2 } } */
/* { dg-final { scan-assembler-times {\mstxvpx?\M}          1 } } */
