#include "scalar-return-dfp_x.h"

T(d32, _Decimal32, 1.2df)

#undef T

void
scalar_return_dfp_1_x ()
{
DEBUG_INIT

#define T(NAME) testit##NAME ();

T(d32)

DEBUG_FINI

if (fails != 0)
  abort ();

#undef T
}
