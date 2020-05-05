/* Provide missing conversions between IEEE 128-bit floating point and Decimal
   floating point for PowerPC.

   Copyright (C) 2020 Free Software Foundation, Inc.
   This file is part of the GNU C Library.
   Contributed by Michael Meissner (meissner@linux.ibm.com)

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   In addition to the permissions in the GNU Lesser General Public
   License, the Free Software Foundation gives you unlimited
   permission to link the compiled version of this file into
   combinations with other programs, and to distribute those
   combinations without any restriction coming from the use of this
   file.  (The Lesser General Public License restrictions do apply in
   other respects; for example, they cover modification of the file,
   and distribution when not linked into a combine executable.)

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */

extern __float128 __dpd_trunctdkf (_Decimal128);
extern __float128 __dpd_extendddkf (_Decimal64);
extern __float128 __dpd_extendsdkf (_Decimal32);
extern _Decimal128 __dpd_extendkftd (__float128);
extern _Decimal64 __dpd_trunckfdd (__float128);
extern _Decimal32 __dpd_trunckfsd (__float128);

extern __ibm128 __dpd_trunctdtf (_Decimal128);
extern __ibm128 __dpd_extendddtf (_Decimal64);
extern __ibm128 __dpd_extendsdtf (_Decimal32);
extern _Decimal128 __dpd_extendtftd (__ibm128);
extern _Decimal64 __dpd_trunctfdd (__ibm128);
extern _Decimal32 __dpd_trunctfsd (__ibm128);

extern __float128 __trunctfkf2 (__ibm128);
extern __ibm128 __extendkftf2 (__float128);

__float128
__dpd_trunctdkf (_Decimal128 x)
{
  __ibm128 ibm = __dpd_trunctdtf (x);
  return __trunctfkf2 (ibm);
}

__float128
__dpd_extendddkf (_Decimal64 x)
{
  __ibm128 ibm = __dpd_extendddtf (x);
  return __trunctfkf2 (ibm);
}

__float128
__dpd_extendsdkf (_Decimal32 x)
{
  __ibm128 ibm = __dpd_extendsdtf (x);
  return __trunctfkf2 (ibm);
}

_Decimal128
__dpd_extendkftd (__float128 x)
{
  __ibm128 ibm = __extendkftf2 (x);
  return __dpd_extendtftd (ibm);
}

_Decimal64
__dpd_trunckfdd (__float128 x)
{
  __ibm128 ibm = __extendkftf2 (x);
  return __dpd_trunctfdd (ibm);
}

_Decimal32
__dpd_trunckfsd (__float128 x)
{
  __ibm128 ibm = __extendkftf2 (x);
  return __dpd_trunctfsd (ibm);
}
