/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power8" } } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-mcpu=power8" } */

int
main ()
{
  __attribute__((altivec(vector__))) unsigned long long test, res;
  const int s0 = 0;
  int mask;

  /* Argument 2 must be 0 or 1.  Argument 3 must be in range 0..15.  */

  /* kelvin comments to be removed:
   *  expands to crypto_vshasegmaw
   *
   * note from altivec.h:
   * #define vec_shasigma_be __builtin_crypto_vshasigma
   *  (kelvin wonders if this macro is conditional upon be?)
   *
   * crypto.md (under "TARGET_CRYPTO" support)
   *  (define_insn "crypto_vshasigma<CR_char>" ... expands to
   *   "vshasigma<CH_char> %0,%1,%2,%3"
   *
   * rs6000-builtin.def has:
   *  BU_CRYPTO_3 (VSHASIGMAW, "vshasigmaw", CONST, crypto_vshasigmaw);
   *  BU_CRYPTO_3 (VSHASIGMAD, "vshasigmad", CONST, crypto_vshasigmad);
   *  BU_CRYPTO_OVERLOAD_3 (VSHASIGMA, "vshasigma")
   *
   * special handling in rs6000.c, if
   *  icode == (CODE_FOR_crypto_vshasigmaw or CODE_FOR_crypto_vshasigmad)
   * within function rs6000_expand_ternop_builtin ()
   */
  res = __builtin_crypto_vshasigmad (test, 1, 0xff); /* { dg-error "argument 3 must be in the range 0..15" } */
  return 0;
}
