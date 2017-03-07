/* Copyright (C) 1989-2017 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

/* This is a temporary specialization of code from libgcc/libgcc2.c.  */

#define COPYSIGN(x,y) __builtin_copysignq (x, y)
#define isnan __builtin_isnan
#define isinf __builtin_isinf

TCtype
__mulkc3 (TFtype a, TFtype b, TFtype c, TFtype d)
{
  TFtype ac, bd, ad, bc, x, y;
  TCtype res;
  TFtype one = 1.0;
  TFtype zero = 0.0;
  TFtype infinity = __builtin_infq ();

  ac = a * c;
  bd = b * d;
  ad = a * d;
  bc = b * c;

  x = ac - bd;
  y = ad + bc;

  if (isnan (x) && isnan (y))
    {
      /* Recover infinities that computed as NaN + iNaN.  */
      _Bool recalc = 0;
      if (isinf (a) || isinf (b))
	{
	  /* z is infinite.  "Box" the infinity and change NaNs in
	     the other factor to 0.  */
	  a = COPYSIGN (isinf (a) ? one : zero, a);
	  b = COPYSIGN (isinf (b) ? one : zero, b);
	  if (isnan (c))
	    c = COPYSIGN (zero, c);

	  if (isnan (d))
	    d = COPYSIGN (zero, d);

          recalc = 1;
	}

     if (isinf (c) || isinf (d))
	{
	  /* w is infinite.  "Box" the infinity and change NaNs in
	     the other factor to 0.  */
	  c = COPYSIGN (isinf (c) ? one : zero, c);
	  d = COPYSIGN (isinf (d) ? one : zero, d);
	  if (isnan (a))
	    a = COPYSIGN (zero, a);

	  if (isnan (b))
	    b = COPYSIGN (zero, b);

	  recalc = 1;
	}

     if (!recalc
	  && (isinf (ac) || isinf (bd)
	      || isinf (ad) || isinf (bc)))
	{
	  /* Recover infinities from overflow by changing NaNs to 0.  */
	  if (isnan (a))
	    a = COPYSIGN (zero, a);

	  if (isnan (b))
	    b = COPYSIGN (zero, b);

	  if (isnan (c))
	    c = COPYSIGN (zero, c);

	  if (isnan (d))
	    d = COPYSIGN (zero, d);

	  recalc = 1;
	}

      if (recalc)
	{
	  x = infinity * (a * c - b * d);
	  y = infinity * (a * d + b * c);
	}
    }

  __real__ res = x;
  __imag__ res = y;
  return res;
}

