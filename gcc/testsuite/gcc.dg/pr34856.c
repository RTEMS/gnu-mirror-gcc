/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-options "-O2 -msse2" { target { i?86-*-* x86_64-*-* } } } */
/* { dg-options "-O2 -maltivec" { target { powerpc*-*-linux* && powerpc_altivec_ok } } } */
/* { dg-skip-if "no vector type for PSImode" { msp430-*-* } { "-mlarge" } { "" } } */

#ifdef __GCC_ARM_CAPABILITY_ANY
typedef unsigned uintoffset_t __attribute__((__mode__(address)));
#else
typedef __UINTPTR_TYPE__ uintoffset_t;
#endif

#undef __vector
#define __vector __attribute__ ((__vector_size__ (16)))

typedef __vector signed char qword;
typedef __vector uintoffset_t VU;

extern short g[192 + 16];

void f (qword);

void f1 (unsigned ctr)
{
  VU pin;

  pin = (VU){(uintoffset_t) &g[16]};
  do
    {
      f ((qword) pin);
      ctr--;
    }
  while (ctr);
}

/* Ignore a warning that is irrelevant to the purpose of this test.  */
/* { dg-prune-output ".*GCC vector passed by reference.*" } */
