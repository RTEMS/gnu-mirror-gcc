/* Copyright (C) 1989-2021 Free Software Foundation, Inc.

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

/* Conversion to IEEE 128-bit floating point from string.  */

/* Enable glibc to declare strfroml if it exists.  */
#define __STDC_WANT_IEC_60559_BFP_EXT__ 1

#include <stddef.h>
#include <stdlib.h>
#include <soft-fp.h>
#include <quad-float128.h>
#include <stdio.h>

/* This function must be built with IBM 128-bit as long double, so that we can
   access the strfroml function if do not have an IEEE 128-bit version, and if
   that is not available, use sprintf.  */
#if !defined(__LONG_DOUBLE_128__) || !defined(__LONG_DOUBLE_IBM128__)
#error "Long double is not IBM 128-bit"
#endif

/* If the user is using GLIBC 2.32, we can use the __snprintfieee128 function.

   If we are linked against an earlier library, we will have fake it by
   converting the value to long double, and using snprinf to do the conversion.
   This isn't ideal, as IEEE 128-bit has more exponent range than IBM
   128-bit.  */

typedef int snprintf_type (char *restrict, size_t, const char *restrict, ...);

extern snprintf_type __snprintfieee128 __attribute__ ((__weak__));

int __strfromkf (char *restrict string,
		 size_t size,
		 const char *restrict format,
		 _Float128 number)
{
  if (__snprintfieee128)
    return __snprintfieee128 (string, size, format, number);

  return snprintf (string, size, format, (long double)number);
}
