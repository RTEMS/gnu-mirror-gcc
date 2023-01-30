/* { dg-require-effective-target cheri_capability_any } */
/* { dg-options "-W -Wall -Wconversion -Wsign-conversion" } */

#include <stddef.h>
#include <string.h>
#include <stdint.h>

int *__capability
intp_good (int *__capability a, __uintcap_t b)
{
  return __builtin_cheri_cap_build (a, b);
}

short *__capability
shortp_good (short *__capability a, __uintcap_t b)
{
  return __builtin_cheri_cap_build (a, b);
}

__intcap_t
intcap_good (__intcap_t a, __uintcap_t b)
{
  return __builtin_cheri_cap_build (a, b);
}

__uintcap_t
uintcap_good (__uintcap_t a, __uintcap_t b)
{
  return __builtin_cheri_cap_build (a, b);
}

int *__capability
intp_zero (int *__capability a)
{
  return __builtin_cheri_cap_build (a, 0);
}

int *__capability
intp_one (int *__capability a)
{
  return __builtin_cheri_cap_build (a, 1);
}

int *__capability
intp_minus_one (int *__capability a)
{
  return __builtin_cheri_cap_build (a, -1); /* { dg-warning {changes value} } */
}

int *__capability
intp_float (int *__capability a, float b)
{
  return __builtin_cheri_cap_build (a, b); /* { dg-warning {may change value} } */
}

__intcap_t
intp_ret_intcap (int *__capability a, __uintcap_t b)
{
  return __builtin_cheri_cap_build (a, b); /* { dg-warning {integer from pointer without a cast} "" { target c } } */
  /* { dg-error {cannot convert} "" { target c++ } .-1 } */
}

short *__capability
intp_ret_shortp (int *__capability a, __uintcap_t b)
{
  return __builtin_cheri_cap_build (a, b); /* { dg-warning {incompatible return type} "" { target c } } */
  /* { dg-error {cannot convert} "" { target c++ } .-1 } */
}

void *__capability
intcap_ret_voidp (__intcap_t a, __uintcap_t b)
{
  return __builtin_cheri_cap_build (a, b); /* { dg-warning {pointer from integer without a cast} "" { target c } } */
  /* { dg-error {cannot convert} "" { target c++ } .-1 } */
}

int *__capability
intp_intcap (int *__capability a, __intcap_t b)
{
  return __builtin_cheri_cap_build (a, b); /* { dg-warning {may change the sign} "" } */
}

int *__capability
intp_intp (int *__capability a, int *__capability b)
{
  return __builtin_cheri_cap_build (a, b); /* { dg-warning {integer from pointer without a cast} "" { target c } } */
  /* { dg-error {cannot convert} "" { target c++ } .-1 } */
}

int *__capability
intp_int64 (int *__capability a, int64_t b)
{
  return __builtin_cheri_cap_build (a, b); /* { dg-warning {may change the sign} "" } */
}

int *__capability
intp_uint64 (int *__capability a, uint64_t b)
{
  return __builtin_cheri_cap_build (a, b);
}

int *__capability
intp_size (int *__capability a, size_t b)
{
  return __builtin_cheri_cap_build (a, b);
}

int
int_bad (int a, __uintcap_t b)
{
  return __builtin_cheri_cap_build (a, b); /* { dg-error {expects a capability type} } */
} /* { dg-bogus {control reaches end} "" { xfail c } } */

float
float_bad (float a, __uintcap_t b)
{
  return __builtin_cheri_cap_build (a, b); /* { dg-error {expects a capability type} } */
} /* { dg-bogus {control reaches end} "" { xfail c } } */

const int *__capability
cintp_good (const int *__capability a, __uintcap_t b)
{
  return __builtin_cheri_cap_build (a, b);
}

int *__capability
cintp_drop_c (const int *__capability a, __uintcap_t b)
{
  return __builtin_cheri_cap_build (a, b); /* { dg-warning {discards 'const' qualifier} "" { target c } } */
  /* { dg-error {invalid conversion} "" { target c++ } .-1 } */
}

volatile int *__capability
vintp_good (volatile int *__capability a, __uintcap_t b)
{
  return __builtin_cheri_cap_build (a, b);
}

int *__capability
vintp_drop_v (volatile int *__capability a, __uintcap_t b)
{
  return __builtin_cheri_cap_build (a, b); /* { dg-warning {discards 'volatile' qualifier} "" { target c } } */
  /* { dg-error {invalid conversion} "" { target c++ } .-1 } */
}

const volatile int *__capability
cvintp_ (const volatile int *__capability a, __uintcap_t b)
{
  return __builtin_cheri_cap_build (a, b);
}

const int *__capability
cvintp_drop_v (const volatile int *__capability a, __uintcap_t b)
{
  return __builtin_cheri_cap_build (a, b); /* { dg-warning {discards 'volatile' qualifier} "" { target c } } */
  /* { dg-error {invalid conversion} "" { target c++ } .-1 } */
}

volatile int *__capability
cvintp_drop_c (const volatile int *__capability a, __uintcap_t b)
{
  return __builtin_cheri_cap_build (a, b); /* { dg-warning {discards 'const' qualifier} "" { target c } } */
  /* { dg-error {invalid conversion} "" { target c++ } .-1 } */
}

int *__capability
cvintp_drop_cv (const volatile int *__capability a, __uintcap_t b)
{
  return __builtin_cheri_cap_build (a, b); /* { dg-warning {discards 'const volatile' qualifier} "" { target c } } */
  /* { dg-error {invalid conversion} "" { target c++ } .-1 } */
}

int *__capability
zero_uintcap_intp (__uintcap_t b)
{
  return __builtin_cheri_cap_build (0, b); /* { dg-warning {not permitted in C\+\+} "" { target c } } */
  /* { dg-error {invalid conversion} "" { target c++ } .-1 } */
}

short *__capability
zero_uintcap_shortp (__uintcap_t b)
{
  return __builtin_cheri_cap_build (0, b); /* { dg-warning {not permitted in C\+\+} "" { target c } } */
  /* { dg-error {invalid conversion} "" { target c++ } .-1 } */
}

__uintcap_t
zero_zero_uintcap (void)
{
  return __builtin_cheri_cap_build (0, 0); /* { dg-warning {integer from pointer without a cast} "" { target c } } */
  /* { dg-error {cannot convert} "" { target c++ } .-1 } */
}

void *__capability
zero_zero_voidp (void)
{
  return __builtin_cheri_cap_build (0, 0);
}

void *__capability
undef_zero_voidp (void)
{
  return __builtin_cheri_cap_build (a, 0); /* { dg-error {'a' (undeclared|was not declared)} } */
} /* { dg-bogus {control reaches end} "" { xfail c } } */

void *__capability
zero_undef_voidp (void)
{
  return __builtin_cheri_cap_build (0, b); /* { dg-error {'b' (undeclared|was not declared)} } */
} /* { dg-bogus {control reaches end} "" { xfail c } } */

void *__capability
undef_undef_voidp (void)
{
  return __builtin_cheri_cap_build (a, b); /* { dg-error {'a' (undeclared|was not declared)} } */
  /* { dg-error {'b' (undeclared|was not declared)} "" { target *-*-* } ".-1" } */
} /* { dg-bogus {control reaches end} "" { xfail c } } */
