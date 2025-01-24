/* { dg-do compile } */
/* { dg-options "-mdejagnu-cpu=power9 -O2" } */

/* PR target/118541 says that the ordered comparison functions like isgreater
   should not optimize floating point conditional moves to use
   x{s,v}cmp{eq,gt,ge}{dp,qp} and xxsel since that instruction can cause traps
   if one of the arguments is a signaling NaN.  */

/* Verify isgreater does not generate xscmpgtdp.  */

double
ordered_compare (double a, double b, double c, double d)
{
  /*
   * fcmpu 0,1,2
   * fmr   1,4
   * bnglr 0
   * fmr   1,3
   * blr
   */

  return __builtin_isgreater (a, b) ? c : d;
}

/* Verify normal > does generate xscmpgtdp.  */

double
normal_compare (double a, double b, double c, double d)
{
  /*
   * xscmpgtdp 1,1,2
   * xxsel     1,4,3,1
   * blr
   */

  return a > b ? c : d;
}

/* { dg-final { scan-assembler-times {\mxscmpg[te]dp\M} }  1 } */
/* { dg-final { scan-assembler-times {\mxxsel} }           1 } */
/* { dg-final { scan-assembler-times {\mxscmpudp\|fcmpu\M} 1 } */

