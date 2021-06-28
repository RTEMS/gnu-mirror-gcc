..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _half-precision:

Half-Precision Floating Point
*****************************

.. index:: half-precision floating point

.. index:: __fp16 data type

On ARM and AArch64 targets, GCC supports half-precision (16-bit) floating
point via the ``__fp16`` type defined in the ARM C Language Extensions.
On ARM systems, you must enable this type explicitly with the
:option:`-mfp16-format` command-line option in order to use it.

ARM targets support two incompatible representations for half-precision
floating-point values.  You must choose one of the representations and
use it consistently in your program.

Specifying :option:`-mfp16-format`:samp:`=ieee` selects the IEEE 754-2008 format.
This format can represent normalized values in the range of 2^{-14} to 65504.
There are 11 bits of significand precision, approximately 3
decimal digits.

Specifying :option:`-mfp16-format`:samp:`=alternative` selects the ARM
alternative format.  This representation is similar to the IEEE
format, but does not support infinities or NaNs.  Instead, the range
of exponents is extended, so that this format can represent normalized
values in the range of 2^{-14} to 131008.

The GCC port for AArch64 only supports the IEEE 754-2008 format, and does
not require use of the :option:`-mfp16-format` command-line option.

The ``__fp16`` type may only be used as an argument to intrinsics defined
in ``<arm_fp16.h>``, or as a storage format.  For purposes of
arithmetic and other operations, ``__fp16`` values in C or C++
expressions are automatically promoted to ``float``.

The ARM target provides hardware support for conversions between
``__fp16`` and ``float`` values
as an extension to VFP and NEON (Advanced SIMD), and from ARMv8-A provides
hardware support for conversions between ``__fp16`` and ``double``
values.  GCC generates code using these hardware instructions if you
compile with options to select an FPU that provides them;
for example, :option:`-mfpu`:samp:`=neon-fp16 -mfloat-abi=softfp`,
in addition to the :option:`-mfp16-format` option to select
a half-precision format.

Language-level support for the ``__fp16`` data type is
independent of whether GCC generates code using hardware floating-point
instructions.  In cases where hardware support is not specified, GCC
implements conversions between ``__fp16`` and other types as library
calls.

It is recommended that portable code use the ``_Float16`` type defined
by ISO/IEC TS 18661-3:2015.  See :ref:`floating-types`.