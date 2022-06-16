/* { dg-require-effective-target cheri_capability_any } */
/* { dg-options "-W -Wall -Wconversion" } */

#include <stddef.h>
#include <string.h>
#include <stdint.h>

struct s { size_t i; };

size_t
no_args (void)
{
  return __builtin_cheri_length_get (); /* { dg-error {too few arguments} } */
} /* { dg-bogus {control reaches end} "" { xfail c } } */

size_t
two_args (__intcap_t x)
{
  return __builtin_cheri_length_get (x, 0); /* { dg-error {too many arguments} } */
}

size_t
intp_good (int *__capability x)
{
  return __builtin_cheri_length_get (x);
}

size_t
shortp_good (short *__capability x)
{
  return __builtin_cheri_length_get (x);
}

size_t
int_bad (int x)
{
  return __builtin_cheri_length_get (x); /* { dg-error {expects a capability type} } */
}

size_t
float_bad (float x)
{
  return __builtin_cheri_length_get (x); /* { dg-error {expects a capability type} } */
}

size_t
s_bad (struct s x)
{
  return __builtin_cheri_length_get (x); /* { dg-error {expects a capability type} } */
}

size_t
intcap_good (__intcap_t x)
{
  return __builtin_cheri_length_get (x);
}

size_t
uintcap_good (__uintcap_t x)
{
  return __builtin_cheri_length_get (x);
}

size_t
zero_good ()
{
  return __builtin_cheri_length_get (0);
}

int64_t
intcap_int64 (__intcap_t x)
{
  return __builtin_cheri_length_get (x); /* { dg-warning {may change the sign} "" { xfail c++ } } */
}

uint32_t
intcap_uint32 (__intcap_t x)
{
  return __builtin_cheri_length_get (x); /* { dg-warning {may change value} } */
}
