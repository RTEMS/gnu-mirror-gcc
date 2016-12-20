/* { dg-do compile { target { powerpc64*-*-* && lp64 } } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power8" } } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-mcpu=power8 -O2" } */

typedef union
{
  float value;
  /* FIXME: Assumes 32 bit int.  */
  unsigned int word;
} ieee_float_shape_type;

/* Get a 32 bit int from a float.  */
#define GET_FLOAT_WORD(i,d)						\
do {									\
  ieee_float_shape_type gf_u;						\
  gf_u.value = (d);							\
  (i) = gf_u.word;							\
} while (0)

int
foo (float d)
{
  int i;

  GET_FLOAT_WORD(i, d);

  return i;
}

/* { dg-final { scan-assembler     "\[ \t\]xscvdpspn " } } */
/* { dg-final { scan-assembler     "\[ \t\]mfvsrd "    } } */
/* { dg-final { scan-assembler     "\[ \t\]sradi "     } } */
/* { dg-final { scan-assembler-not "\[ \t\]stfs "      } } */
/* { dg-final { scan-assembler-not "\[ \t\]ori 2,2,0"  } } */
/* { dg-final { scan-assembler-not "\[ \t\]lwa "       } } */
