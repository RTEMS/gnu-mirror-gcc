..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _soft-float-library-routines:

Routines for floating point emulation
*************************************

.. index:: soft float library

.. index:: arithmetic library

.. index:: math library

.. index:: msoft-float

The software floating point library is used on machines which do not
have hardware support for floating point.  It is also used whenever
:option:`-msoft-float` is used to disable generation of floating point
instructions.  (Not all targets support this switch.)

For compatibility with other compilers, the floating point emulation
routines can be renamed with the ``DECLARE_LIBRARY_RENAMES`` macro
(see :ref:`library-calls`).  In this section, the default names are used.

Presently the library does not support ``XFmode``, which is used
for ``long double`` on some architectures.

Arithmetic functions
^^^^^^^^^^^^^^^^^^^^

.. function:: float __addsf3 (float a, float b)

.. function:: double __adddf3 (double a, double b)

.. function:: long double __addtf3 (long double a, long double b)

.. function:: long double __addxf3 (long double a, long double b)

  These functions return the sum of :samp:`{a}` and :samp:`{b}`.

.. function:: float __subsf3 (float a, float b)

.. function:: double __subdf3 (double a, double b)

.. function:: long double __subtf3 (long double a, long double b)

.. function:: long double __subxf3 (long double a, long double b)

  These functions return the difference between :samp:`{b}` and :samp:`{a}` ;
  that is, :samp:`{a}` - :samp:`{b}`.

.. function:: float __mulsf3 (float a, float b)

.. function:: double __muldf3 (double a, double b)

.. function:: long double __multf3 (long double a, long double b)

.. function:: long double __mulxf3 (long double a, long double b)

  These functions return the product of :samp:`{a}` and :samp:`{b}`.

.. function:: float __divsf3 (float a, float b)

.. function:: double __divdf3 (double a, double b)

.. function:: long double __divtf3 (long double a, long double b)

.. function:: long double __divxf3 (long double a, long double b)

  These functions return the quotient of :samp:`{a}` and :samp:`{b}` ; that is,
  :samp:`{a}` / :samp:`{b}`.

.. function:: float __negsf2 (float a)

.. function:: double __negdf2 (double a)

.. function:: long double __negtf2 (long double a)

.. function:: long double __negxf2 (long double a)

  These functions return the negation of :samp:`{a}`.  They simply flip the
  sign bit, so they can produce negative zero and negative NaN.

Conversion functions
^^^^^^^^^^^^^^^^^^^^

.. function:: double __extendsfdf2 (float a)

.. function:: long double __extendsftf2 (float a)

.. function:: long double __extendsfxf2 (float a)

.. function:: long double __extenddftf2 (double a)

.. function:: long double __extenddfxf2 (double a)

  These functions extend :samp:`{a}` to the wider mode of their return
  type.

.. function:: double __truncxfdf2 (long double a)

.. function:: double __trunctfdf2 (long double a)

.. function:: float __truncxfsf2 (long double a)

.. function:: float __trunctfsf2 (long double a)

.. function:: float __truncdfsf2 (double a)

  These functions truncate :samp:`{a}` to the narrower mode of their return
  type, rounding toward zero.

.. function:: int __fixsfsi (float a)

.. function:: int __fixdfsi (double a)

.. function:: int __fixtfsi (long double a)

.. function:: int __fixxfsi (long double a)

  These functions convert :samp:`{a}` to a signed integer, rounding toward zero.

.. function:: long __fixsfdi (float a)

.. function:: long __fixdfdi (double a)

.. function:: long __fixtfdi (long double a)

.. function:: long __fixxfdi (long double a)

  These functions convert :samp:`{a}` to a signed long, rounding toward zero.

.. function:: long long __fixsfti (float a)

.. function:: long long __fixdfti (double a)

.. function:: long long __fixtfti (long double a)

.. function:: long long __fixxfti (long double a)

  These functions convert :samp:`{a}` to a signed long long, rounding toward zero.

.. function:: unsigned int __fixunssfsi (float a)

.. function:: unsigned int __fixunsdfsi (double a)

.. function:: unsigned int __fixunstfsi (long double a)

.. function:: unsigned int __fixunsxfsi (long double a)

  These functions convert :samp:`{a}` to an unsigned integer, rounding
  toward zero.  Negative values all become zero.

.. function:: unsigned long __fixunssfdi (float a)

.. function:: unsigned long __fixunsdfdi (double a)

.. function:: unsigned long __fixunstfdi (long double a)

.. function:: unsigned long __fixunsxfdi (long double a)

  These functions convert :samp:`{a}` to an unsigned long, rounding
  toward zero.  Negative values all become zero.

.. function:: unsigned long long __fixunssfti (float a)

.. function:: unsigned long long __fixunsdfti (double a)

.. function:: unsigned long long __fixunstfti (long double a)

.. function:: unsigned long long __fixunsxfti (long double a)

  These functions convert :samp:`{a}` to an unsigned long long, rounding
  toward zero.  Negative values all become zero.

.. function:: float __floatsisf (int i)

.. function:: double __floatsidf (int i)

.. function:: long double __floatsitf (int i)

.. function:: long double __floatsixf (int i)

  These functions convert :samp:`{i}`, a signed integer, to floating point.

.. function:: float __floatdisf (long i)

.. function:: double __floatdidf (long i)

.. function:: long double __floatditf (long i)

.. function:: long double __floatdixf (long i)

  These functions convert :samp:`{i}`, a signed long, to floating point.

.. function:: float __floattisf (long long i)

.. function:: double __floattidf (long long i)

.. function:: long double __floattitf (long long i)

.. function:: long double __floattixf (long long i)

  These functions convert :samp:`{i}`, a signed long long, to floating point.

.. function:: float __floatunsisf (unsigned int i)

.. function:: double __floatunsidf (unsigned int i)

.. function:: long double __floatunsitf (unsigned int i)

.. function:: long double __floatunsixf (unsigned int i)

  These functions convert :samp:`{i}`, an unsigned integer, to floating point.

.. function:: float __floatundisf (unsigned long i)

.. function:: double __floatundidf (unsigned long i)

.. function:: long double __floatunditf (unsigned long i)

.. function:: long double __floatundixf (unsigned long i)

  These functions convert :samp:`{i}`, an unsigned long, to floating point.

.. function:: float __floatuntisf (unsigned long long i)

.. function:: double __floatuntidf (unsigned long long i)

.. function:: long double __floatuntitf (unsigned long long i)

.. function:: long double __floatuntixf (unsigned long long i)

  These functions convert :samp:`{i}`, an unsigned long long, to floating point.

Comparison functions
^^^^^^^^^^^^^^^^^^^^

There are two sets of basic comparison functions.

.. function:: int __cmpsf2 (float a, float b)

.. function:: int __cmpdf2 (double a, double b)

.. function:: int __cmptf2 (long double a, long double b)

  These functions calculate a <=> b.  That is, if :samp:`{a}` is less
  than :samp:`{b}`, they return -1; if :samp:`{a}` is greater than :samp:`{b}`, they
  return 1; and if :samp:`{a}` and :samp:`{b}` are equal they return 0.  If
  either argument is NaN they return 1, but you should not rely on this;
  if NaN is a possibility, use one of the higher-level comparison
  functions.

.. function:: int __unordsf2 (float a, float b)

.. function:: int __unorddf2 (double a, double b)

.. function:: int __unordtf2 (long double a, long double b)

  These functions return a nonzero value if either argument is NaN, otherwise 0.

There is also a complete group of higher level functions which
correspond directly to comparison operators.  They implement the ISO C
semantics for floating-point comparisons, taking NaN into account.
Pay careful attention to the return values defined for each set.
Under the hood, all of these routines are implemented as

.. code-block:: c++

    if (__unordXf2 (a, b))
      return E;
    return __cmpXf2 (a, b);

where :samp:`{E}` is a constant chosen to give the proper behavior for
NaN.  Thus, the meaning of the return value is different for each set.
Do not rely on this implementation; only the semantics documented
below are guaranteed.

.. function:: int __eqsf2 (float a, float b)

.. function:: int __eqdf2 (double a, double b)

.. function:: int __eqtf2 (long double a, long double b)

  These functions return zero if neither argument is NaN, and :samp:`{a}` and
  :samp:`{b}` are equal.

.. function:: int __nesf2 (float a, float b)

.. function:: int __nedf2 (double a, double b)

.. function:: int __netf2 (long double a, long double b)

  These functions return a nonzero value if either argument is NaN, or
  if :samp:`{a}` and :samp:`{b}` are unequal.

.. function:: int __gesf2 (float a, float b)

.. function:: int __gedf2 (double a, double b)

.. function:: int __getf2 (long double a, long double b)

  These functions return a value greater than or equal to zero if
  neither argument is NaN, and :samp:`{a}` is greater than or equal to
  :samp:`{b}`.

.. function:: int __ltsf2 (float a, float b)

.. function:: int __ltdf2 (double a, double b)

.. function:: int __lttf2 (long double a, long double b)

  These functions return a value less than zero if neither argument is
  NaN, and :samp:`{a}` is strictly less than :samp:`{b}`.

.. function:: int __lesf2 (float a, float b)

.. function:: int __ledf2 (double a, double b)

.. function:: int __letf2 (long double a, long double b)

  These functions return a value less than or equal to zero if neither
  argument is NaN, and :samp:`{a}` is less than or equal to :samp:`{b}`.

.. function:: int __gtsf2 (float a, float b)

.. function:: int __gtdf2 (double a, double b)

.. function:: int __gttf2 (long double a, long double b)

  These functions return a value greater than zero if neither argument
  is NaN, and :samp:`{a}` is strictly greater than :samp:`{b}`.

Other floating-point functions
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. function:: float __powisf2 (float a, int b)

.. function:: double __powidf2 (double a, int b)

.. function:: long double __powitf2 (long double a, int b)

.. function:: long double __powixf2 (long double a, int b)

  These functions convert raise :samp:`{a}` to the power :samp:`{b}`.

.. function:: complex float __mulsc3 (float a, float b, float c, float d)

.. function:: complex double __muldc3 (double a, double b, double c, double d)

.. function:: complex long double __multc3 (long double a, long double b, long double c, long double d)

.. function:: complex long double __mulxc3 (long double a, long double b, long double c, long double d)

  These functions return the product of :samp:`{a}` + i :samp:`{b}` and
  :samp:`{c}` + i :samp:`{d}`, following the rules of C99 Annex G.

.. function:: complex float __divsc3 (float a, float b, float c, float d)

.. function:: complex double __divdc3 (double a, double b, double c, double d)

.. function:: complex long double __divtc3 (long double a, long double b, long double c, long double d)

.. function:: complex long double __divxc3 (long double a, long double b, long double c, long double d)

  These functions return the quotient of :samp:`{a}` + i :samp:`{b}` and
  :samp:`{c}` + i :samp:`{d}` (i.e., ( :samp:`{a}` + i :samp:`{b}` ) / ( :samp:`{c}`
  + i :samp:`{d}` )), following the rules of C99 Annex G.