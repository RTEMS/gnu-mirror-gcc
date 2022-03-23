/* { dg-require-effective-target int128     } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -O2" } */

/* This test makes sure that GCC generates the maddhd, maddhdu, and maddld
   power9 instructions when doing some forms of 64-bit integers converted to
   128-bit integers and used with multiply/add operations.  */

__int128_t
s_mult_add (long long a,
	    long long b,
	    long long c)
{
  /* maddhd, maddld.  */
  return ((__int128_t)a * (__int128_t)b) + (__int128_t)c;
}

/* Test 32-bit constants that are loaded into GPRs instead of doing the
   mulld/mulhd and then addic/addime or addc/addze.  */
__int128_t
s_mult_add_m10 (long long a,
		long long b)
{
  /* maddhd, maddld.  */
  return ((__int128_t)a * (__int128_t)b) - 10;
}

__int128_t
s_mult_add_70000 (long long a,
		  long long b)
{
  /* maddhd, maddld.  */
  return ((__int128_t)a * (__int128_t)b) + 70000;
}

__uint128_t
u_mult_add (unsigned long long a,
	    unsigned long long b,
	    unsigned long long c)
{
  /* maddhd, maddld.  */
  return ((__uint128_t)a * (__uint128_t)b) + (__uint128_t)c;
}

__uint128_t
u_mult_add_0x80000000 (unsigned long long a,
		       unsigned long long b)
{
  /* maddhd, maddld.  */
  return ((__uint128_t)a * (__uint128_t)b) + 0x80000000UL;
}

/* { dg-final { scan-assembler-not   {\maddc\M}      } } */
/* { dg-final { scan-assembler-not   {\madde\M}      } } */
/* { dg-final { scan-assembler-not   {\maddid\M}     } } */
/* { dg-final { scan-assembler-not   {\maddme\M}     } } */
/* { dg-final { scan-assembler-not   {\maddze\M}     } } */
/* { dg-final { scan-assembler-not   {\mmulhd\M}     } } */
/* { dg-final { scan-assembler-not   {\mmulld\M}     } } */
/* { dg-final { scan-assembler-times {\mmaddhd\M}  3 } } */
/* { dg-final { scan-assembler-times {\mmaddhdu\M} 2 } } */
/* { dg-final { scan-assembler-times {\mmaddld\M}  5 } } */
