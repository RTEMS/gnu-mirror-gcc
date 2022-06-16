/* { dg-require-effective-target cheri_capability_any } */
/* { dg-options "-W -Wall -Wconversion" } */

#include <stddef.h>
#include <string.h>
#include <stdint.h>

struct s { size_t i; };

int *__capability
one_arg (int *__capability a)
{
  return __builtin_cheri_bounds_set (a); /* { dg-error {too few arguments} } */
} /* { dg-bogus {control reaches end} "" { xfail c } } */

int *__capability
three_args (int *__capability a, size_t b)
{
  return __builtin_cheri_bounds_set (a, b, 0); /* { dg-error {too many arguments} } */
}

int *__capability
intp_good (int *__capability a, size_t b)
{
  return __builtin_cheri_bounds_set (a, b);
}

short *__capability
shortp_good (short *__capability a, size_t b)
{
  return __builtin_cheri_bounds_set (a, b);
}

struct s *__capability
sp_good (struct s *__capability a, size_t b)
{
  return __builtin_cheri_bounds_set (a, b);
}

int *__capability
intp_int64_t (int *__capability a, int64_t b)
{
  return __builtin_cheri_bounds_set (a, b); /* { dg-warning {may change the sign} "" { xfail c++ } } */
}

int *__capability
intp_uint64_t (int *__capability a, uint64_t b)
{
  return __builtin_cheri_bounds_set (a, b);
}

int *__capability
intp_intcap (int *__capability a, __intcap_t b)
{
  return __builtin_cheri_bounds_set (a, b); /* { dg-warning {may change the sign} "" { xfail c++ } } */
}

int *__capability
intp_uintcap (int *__capability a, __uintcap_t b)
{
  return __builtin_cheri_bounds_set (a, b);
}

int *__capability
intp_float (int *__capability a, float b)
{
  return __builtin_cheri_bounds_set (a, b); /* { dg-warning {may change value} } */
}

int *__capability
intp_s (int *__capability a, struct s b)
{
  return __builtin_cheri_bounds_set (a, b); /* { dg-error {(incompatible type|cannot convert)} } */
} /* { dg-bogus {control reaches end} "" { xfail c } } */

__intcap_t
intp_to_intcap (int *__capability a, size_t b)
{
  return __builtin_cheri_bounds_set (a, b); /* { dg-warning {integer from pointer without a cast} "" { target c } } */
  /* { dg-error {cannot convert} "" { target c++ } .-1 } */
}

int
intp_to_int (int *__capability a, size_t b)
{
  return __builtin_cheri_bounds_set (a, b); /* { dg-warning {integer from pointer without a cast} "" { target c } } */
  /* { dg-error {invalid conversion} "" { target c++ } .-1 } */
}

void *__capability
intp_to_voidp (int *__capability a, size_t b)
{
  return __builtin_cheri_bounds_set (a, b);
}

short *__capability
intp_to_shortp (int *__capability a, size_t b)
{
  return __builtin_cheri_bounds_set (a, b); /* { dg-warning {incompatible return type} "" { target c } } */
  /* { dg-error {cannot convert} "" { target c++ } .-1 } */
}

__intcap_t
intcap_good (__intcap_t a, size_t b)
{
  return __builtin_cheri_bounds_set (a, b);
}

__uintcap_t
intcap_to_uintcap (__intcap_t a, size_t b)
{
  return __builtin_cheri_bounds_set (a, b); /* { dg-warning {may change the sign} "" { xfail c++ } } */
}

int *__capability
intcap_to_intp (__intcap_t a, size_t b)
{
  return __builtin_cheri_bounds_set (a, b); /* { dg-warning {pointer from integer without a cast} "" { target c } } */
  /* { dg-error {cannot convert} "" { target c++ } .-1 } */
}

__uintcap_t
uintcap_good (__uintcap_t a, size_t b)
{
  return __builtin_cheri_bounds_set (a, b);
}

__intcap_t
uintcap_to_intcap (__uintcap_t a, size_t b)
{
  return __builtin_cheri_bounds_set (a, b); /* { dg-warning {may change the sign} "" { xfail c++ } } */
}

__intcap_t
int_to_intcap (int a, size_t b)
{
  return __builtin_cheri_bounds_set (a, b); /* { dg-error {expects a capability type} } */
} /* { dg-bogus {control reaches end} "" { xfail c } } */

const int *__capability
cintp_good (const int *__capability a, size_t b)
{
  return __builtin_cheri_bounds_set (a, b);
}

int *__capability
cintp_drop_c (const int *__capability a, size_t b)
{
  return __builtin_cheri_bounds_set (a, b); /* { dg-warning {discards 'const' qualifier} "" { target c } } */
  /* { dg-error {invalid conversion} "" { target c++ } .-1 } */
}

volatile int *__capability
vintp_good (volatile int *__capability a, size_t b)
{
  return __builtin_cheri_bounds_set (a, b);
}

int *__capability
vintp_drop_v (volatile int *__capability a, size_t b)
{
  return __builtin_cheri_bounds_set (a, b); /* { dg-warning {discards 'volatile' qualifier} "" { target c } } */
  /* { dg-error {invalid conversion} "" { target c++ } .-1 } */
}

const volatile int *__capability
cvintp_good (const volatile int *__capability a, size_t b)
{
  return __builtin_cheri_bounds_set (a, b);
}

const int *__capability
cvintp_drop_v (const volatile int *__capability a, size_t b)
{
  return __builtin_cheri_bounds_set (a, b); /* { dg-warning {discards 'volatile' qualifier} "" { target c } } */
  /* { dg-error {invalid conversion} "" { target c++ } .-1 } */
}

volatile int *__capability
cvintp_drop_c (const volatile int *__capability a, size_t b)
{
  return __builtin_cheri_bounds_set (a, b); /* { dg-warning {discards 'const' qualifier} "" { target c } } */
  /* { dg-error {invalid conversion} "" { target c++ } .-1 } */
}

int *__capability
cvintp_drop_cv (const volatile int *__capability a, size_t b)
{
  return __builtin_cheri_bounds_set (a, b); /* { dg-warning {discards 'const volatile' qualifier} "" { target c } } */
  /* { dg-error {invalid conversion} "" { target c++ } .-1 } */
}

int *__capability
zero_intp (size_t b)
{
  return __builtin_cheri_bounds_set (0, b); /* { dg-warning {not permitted in C\+\+} "" { target c } } */
  /* { dg-error {invalid conversion} "" { target c++ } .-1 } */
}

short *__capability
zero_shortp (size_t b)
{
  return __builtin_cheri_bounds_set (0, b); /* { dg-warning {not permitted in C\+\+} "" { target c } } */
  /* { dg-error {invalid conversion} "" { target c++ } .-1 } */
}

void *
undef_zero_voidp (void)
{
  return __builtin_cheri_bounds_set (a, 0); /* { dg-error {'a' (undeclared|was not declared)} } */
} /* { dg-bogus {control reaches end} "" { xfail c } } */

void *__capability
zero_undef_voidp (void)
{
  return __builtin_cheri_bounds_set (0, b); /* { dg-error {'b' (undeclared|was not declared)} } */
} /* { dg-bogus {control reaches end} "" { xfail c } } */

void *__capability
undef_undef_voidp (void)
{
  return __builtin_cheri_bounds_set (a, b); /* { dg-error {'a' (undeclared|was not declared)} } */
  /* { dg-error {'b' (undeclared|was not declared)} "" { target *-*-* } ".-1" } */
} /* { dg-bogus {control reaches end} "" { xfail c } } */
