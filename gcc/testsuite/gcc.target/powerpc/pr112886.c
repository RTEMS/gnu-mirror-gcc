/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -O2" } */

/* PR target/112886: Test that print_operand %S<n> gives the correct register
   number for VSX registers (i.e. if the register is an Altivec register, the
   register number is 32..63 instead of 0..31.  */

void
test (__vector_pair *p, __vector_pair *q, __vector_pair *r)
{
  __asm__ ("xvadddp %x0,%x1,%x2\n\txvadddp %S0,%S1,%S2"
	   : "=v" (*p)
	   : "v" (*q), "v" (*r));
}

/* { dg-final { scan-assembler-times {\mxvadddp (3[2-9]|[45][0-9]|6[0-3]),(3[2-9]|[45][0-9]|6[0-3]),(3[2-9]|[45][0-9]|6[0-3])\M} 2 } } */
/* { dg-final { scan-assembler-times {\mlxvp\M}  2 } } */
/* { dg-final { scan-assembler-times {\mstxvp\M} 1 } } */
