/* Operations on HOST_WIDE_INT.
   Copyright (C) 1987-2022 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"

/* Compute the greatest common divisor of two numbers A and B using
   Euclid's algorithm.  */

HOST_WIDE_INT
gcd (HOST_WIDE_INT a, HOST_WIDE_INT b)
{
  HOST_WIDE_INT x, y, z;

  x = abs_hwi (a);
  y = abs_hwi (b);

  while (x > 0)
    {
      z = y % x;
      y = x;
      x = z;
    }

  return y;
}

/* For X and Y positive integers, return X multiplied by Y and check
   that the result does not overflow.  */

HOST_WIDE_INT
pos_mul_hwi (HOST_WIDE_INT x, HOST_WIDE_INT y)
{
  if (x != 0)
    gcc_checking_assert ((HOST_WIDE_INT_MAX) / x >= y);

  return x * y;
}

/* Return X multiplied by Y and check that the result does not
   overflow.  */

HOST_WIDE_INT
mul_hwi (HOST_WIDE_INT x, HOST_WIDE_INT y)
{
  gcc_checking_assert (x != HOST_WIDE_INT_MIN
		       && y != HOST_WIDE_INT_MIN);

  if (x >= 0)
    {
      if (y >= 0)
	return pos_mul_hwi (x, y);

      return -pos_mul_hwi (x, -y);
    }

  if (y >= 0)
    return -pos_mul_hwi (-x, y);

  return pos_mul_hwi (-x, -y);
}

/* Compute the least common multiple of two numbers A and B .  */

HOST_WIDE_INT
least_common_multiple (HOST_WIDE_INT a, HOST_WIDE_INT b)
{
  return mul_hwi (abs_hwi (a) / gcd (a, b), abs_hwi (b));
}
