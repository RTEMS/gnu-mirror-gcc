/* { dg-require-effective-target cheri_capability_any } */
/* { dg-options "-W -Wall -Wconversion" } */

#include <stddef.h>
#include <string.h>

int *__capability
intp_shortp_good (int *__capability a, short *__capability b)
{
  return __builtin_cheri_seal (a, b);
}

int *__capability
intp_intcap_good (int *__capability a, __intcap_t b)
{
  return __builtin_cheri_seal (a, b);
}

short *__capability
shortp_shortp_good (short *__capability a, short *__capability b)
{
  return __builtin_cheri_seal (a, b);
}

short *__capability
shortp_intcap_good (short *__capability a, __intcap_t b)
{
  return __builtin_cheri_seal (a, b);
}

__intcap_t
intcap_voidp_good (__intcap_t a, void *__capability b)
{
  return __builtin_cheri_seal (a, b);
}

int *__capability
intp_size (int *__capability a, size_t b)
{
  return __builtin_cheri_seal (a, b); /* { dg-error {expects a capability type} } */
}

int *__capability
intp_zero (int *__capability a)
{
  return __builtin_cheri_seal (a, 0);
}

int *__capability
intp_float (int *__capability a, float b)
{
  return __builtin_cheri_seal (a, b); /* { dg-error {expects a capability type} } */
}

__intcap_t
intp_ret_intcap (int *__capability a, __intcap_t b)
{
  return __builtin_cheri_seal (a, b); /* { dg-warning {integer from pointer without a cast} "" { target c } } */
  /* { dg-error {cannot convert} "" { target c++ } .-1 } */
}

short *__capability
intp_ret_shortp (int *__capability a, short *__capability b)
{
  return __builtin_cheri_seal (a, b); /* { dg-warning {incompatible return type} "" { target c } } */
  /* { dg-error {cannot convert} "" { target c++ } .-1 } */
}

void *__capability
intcap_ret_voidp (__intcap_t a, __intcap_t b)
{
  return __builtin_cheri_seal (a, b); /* { dg-warning {pointer from integer without a cast} "" { target c } } */
  /* { dg-error {cannot convert} "" { target c++ } .-1 } */
}

int
int_bad (int a, __intcap_t b)
{
  return __builtin_cheri_seal (a, b); /* { dg-error {expects a capability type} } */
} /* { dg-bogus {control reaches end} "" { xfail c } } */

float
float_bad (float a, __intcap_t b)
{
  return __builtin_cheri_seal (a, b); /* { dg-error {expects a capability type} } */
} /* { dg-bogus {control reaches end} "" { xfail c } } */

const int *__capability
cintp_good (const int *__capability a, __intcap_t b)
{
  return __builtin_cheri_seal (a, b);
}

int *__capability
cintp_drop_c (const int *__capability a, __intcap_t b)
{
  return __builtin_cheri_seal (a, b); /* { dg-warning {discards 'const' qualifier} "" { target c } } */
  /* { dg-error {invalid conversion} "" { target c++ } .-1 } */
}

volatile int *__capability
vintp_good (volatile int *__capability a, __intcap_t b)
{
  return __builtin_cheri_seal (a, b);
}

int *__capability
vintp_drop_v (volatile int *__capability a, __intcap_t b)
{
  return __builtin_cheri_seal (a, b); /* { dg-warning {discards 'volatile' qualifier} "" { target c } } */
  /* { dg-error {invalid conversion} "" { target c++ } .-1 } */
}

const volatile int *__capability
cvintp_ (const volatile int *__capability a, __intcap_t b)
{
  return __builtin_cheri_seal (a, b);
}

const int *__capability
cvintp_drop_v (const volatile int *__capability a, __intcap_t b)
{
  return __builtin_cheri_seal (a, b); /* { dg-warning {discards 'volatile' qualifier} "" { target c } } */
  /* { dg-error {invalid conversion} "" { target c++ } .-1 } */
}

volatile int *__capability
cvintp_drop_c (const volatile int *__capability a, __intcap_t b)
{
  return __builtin_cheri_seal (a, b); /* { dg-warning {discards 'const' qualifier} "" { target c } } */
  /* { dg-error {invalid conversion} "" { target c++ } .-1 } */
}

int *__capability
cvintp_drop_cv (const volatile int *__capability a, __intcap_t b)
{
  return __builtin_cheri_seal (a, b); /* { dg-warning {discards 'const volatile' qualifier} "" { target c } } */
  /* { dg-error {invalid conversion} "" { target c++ } .-1 } */
}

int *__capability
zero_intcap_intp (__intcap_t b)
{
  return __builtin_cheri_seal (0, b); /* { dg-warning {not permitted in C\+\+} "" { target c } } */
  /* { dg-error {invalid conversion} "" { target c++ } .-1 } */
}

short *__capability
zero_intcap_shortp (__intcap_t b)
{
  return __builtin_cheri_seal (0, b); /* { dg-warning {not permitted in C\+\+} "" { target c } } */
  /* { dg-error {invalid conversion} "" { target c++ } .-1 } */
}

__intcap_t
zero_zero_intcap (void)
{
  return __builtin_cheri_seal (0, 0); /* { dg-warning {integer from pointer without a cast} "" { target c } } */
  /* { dg-error {cannot convert} "" { target c++ } .-1 } */
}

void *__capability
zero_zero_voidp (void)
{
  return __builtin_cheri_seal (0, 0);
}

void *__capability
undef_zero_voidp (void)
{
  return __builtin_cheri_seal (a, 0); /* { dg-error {'a' (undeclared|was not declared)} } */
} /* { dg-bogus {control reaches end} "" { xfail c } } */

void *__capability
zero_undef_voidp (void)
{
  return __builtin_cheri_seal (0, b); /* { dg-error {'b' (undeclared|was not declared)} } */
} /* { dg-bogus {control reaches end} "" { xfail c } } */

void *__capability
undef_undef_voidp (void)
{
  return __builtin_cheri_seal (a, b); /* { dg-error {'a' (undeclared|was not declared)} } */
  /* { dg-error {'b' (undeclared|was not declared)} "" { target *-*-* } ".-1" } */
} /* { dg-bogus {control reaches end} "" { xfail c } } */
