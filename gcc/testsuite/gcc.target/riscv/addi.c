/* { dg-do compile } */
/* { dg-options "-march=rv64gc -mabi=lp64" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" } } */

long long f (long long a)
{
  //	addi	a0,a0,2047
  //	addi	a0,a0,1

  return a + 2048;
}

long long f2 (long long a)
{
  //	addi	a0,a0,2047
  //	addi	a0,a0,398

  return a + 2445;
}

long long f3 (long long a)
{
  //  	addi	a0,a0,-2048
  //	addi	a0,a0,-397

  return a - 2445;
}

long long f6 (long long a)
{
  //  	li	a5,1179648
  //	add	a0,a0,a5

  return a + (0x12 << 16);
}

/* { dg-final { scan-assembler-times "addi\t" 6 } } */
/* { dg-final { scan-assembler-times "li\t" 1 } } */
/* { dg-final { scan-assembler-times "add\t" 1 } } */
