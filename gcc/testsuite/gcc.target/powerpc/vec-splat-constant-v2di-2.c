/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -O2 -mxxsplti32dx -mxxspltiw" } */

/* Test generation of integer constants loaded into the vector registers with
   the ISA 3.1 (power10) instruction XXSPLTI32DX.  */

#define LARGE_BITS	0x12345678ABCDEF01LL
#define SUBNORMAL	0x8000000000000001LL

/* 0x8000000000000001LL is the bit pattern for a negative subnormal value can
   be generated with XXSPLTI32DX but not XXSLTIDP.  */
vector long long
vector_float_subnormal (void)
{
  /* 2x XXSPLTI32DX.  */
  return (vector long long) { SUBNORMAL, SUBNORMAL };
}

/* 0x12345678ABCDEF01LL is a large constant that can be loaded with 2x
   XXSPLTI32DX instructions.  */
vector long long
vector_large_constant (void)
{
  /* 2x XXSPLTI32DX.  */
  return (vector long long) { LARGE_BITS, LARGE_BITS };
}

/* { dg-final { scan-assembler-times {\mxxsplti32dx\M} 4 } } */
