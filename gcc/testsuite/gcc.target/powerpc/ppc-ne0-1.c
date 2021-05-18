/* PR target/51274 */
/* { dg-do compile } */
/* { dg-options "-O2 -mno-isel" } */

/* If the compiler was configured to automatically generate power10 support with
   --with-cpu=power10, turn it off.  Otherwise, it will generate a SETBCR
   instruction instead of ADDIC/SUBFE.  */

#ifdef _ARCH_PWR10
#pragma GCC target ("cpu=power9")
#endif

/* { dg-final { scan-assembler-times "addic" 4 } } */
/* { dg-final { scan-assembler-times "subfe" 1 } } */
/* { dg-final { scan-assembler-times "addze" 3 } } */

long ne0(long a)
{
  return a != 0;
}

long plus_ne0(long a, long b)
{
  return (a != 0) + b;
}

void dummy(void);

void cmp_plus_ne0(long a, long b)
{
  if ((a != 0) + b)
    dummy();
}

long plus_ne0_cmp(long a, long b)
{
  a = (a != 0) + b;
  if (a)
    dummy();
  return a;
}
