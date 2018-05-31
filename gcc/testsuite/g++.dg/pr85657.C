// { dg-do compile { target { powerpc*-*-linux* } } }
// { dg-require-effective-target ppc_float128_sw }
// { dg-options "-mvsx -mfloat128 -O2 -mabi=ibmlongdouble -Wno-psabi" }

// PR 85657
// Check that __ibm128 and long double are represented as different types, even
// if long double is currently using the same representation as __ibm128.

template <class __T> inline bool
iszero (__T __val)
{
  return __val == 0;
}

int
use_template (long double *p_ld,
	      __ibm128 *p_ibm128,
	      __float128 *p_flt128)
{
  long double ld = *p_ld;
  __ibm128 ibm = *p_ibm128;
  __float128 flt = *p_flt128;

#ifdef _ARCH_PWR7
  __asm__ (" # %x0, %x1, %x2" : "+d" (ld), "+d" (ibm), "+wa" (flt));
#endif

  return iszero (ld) + iszero (ibm) + iszero (flt);
}
