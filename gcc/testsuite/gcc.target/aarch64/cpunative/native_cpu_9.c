/* { dg-do compile { target { { aarch64*-*-linux*} && native } } } */
/* { dg-set-compiler-env-var GCC_CPUINFO "$srcdir/gcc.target/aarch64/cpunative/info_9" } */
/* { dg-additional-options "-mcpu=native" } */
/* { dg-skip-if "SVE is not supported with capabilities" { aarch64_capability_any } } */

int main()
{
  return 0;
}

/* { dg-final { scan-assembler {\.arch armv8-a\+sve2-sm4} } } */

/* Test one here a feature that is a prefix of another is enabled.
   In this case sve is a prefix to svesm4, but sve2-sm4 should be
   enabled.  */
