..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _enabling-openacc:

Enabling OpenACC
----------------

To activate the OpenACC extensions for C/C++ and Fortran, the compile-time
flag :option:`-fopenacc` must be specified.  This enables the OpenACC directive
``#pragma acc`` in C/C++ and ``!$acc`` directives in free form,
``c$acc``, ``*$acc`` and ``!$acc`` directives in fixed form,
``!$`` conditional compilation sentinels in free form and ``c$``,
``*$`` and ``!$`` sentinels in fixed form, for Fortran.  The flag also
arranges for automatic linking of the OpenACC runtime library
(OpenACC Runtime Library Routines).

See https://gcc.gnu.org/wiki/OpenACC for more information.

A complete description of all OpenACC directives accepted may be found in
the `OpenACC <https://www.openacc.org>`_ Application Programming
Interface manual, version 2.6.

.. -
   OpenACC Runtime Library Routines
   -