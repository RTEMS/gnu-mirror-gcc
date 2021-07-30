/* { dg-do compile { target { { aarch64*-*-linux*} && native } } } */
/* { dg-set-compiler-env-var GCC_CPUINFO "$srcdir/gcc.target/aarch64/cpunative/info_8" } */
/* { dg-additional-options "-mcpu=native" } */
/* { dg-skip-if "SVE is not supported with capabilities" { aarch64_capability_any } } */

int main()
{
  return 0;
}

/* { dg-final { scan-assembler {\.arch armv8-a\+sve} } } */

/* Test one where sve is enabled.  */
