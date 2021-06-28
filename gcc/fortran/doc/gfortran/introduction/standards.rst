..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _standards:

Standards
*********

.. index:: Standards

.. toctree::
  :maxdepth: 2


The GNU Fortran compiler implements
ISO/IEC 1539:1997 (Fortran 95).  As such, it can also compile essentially all
standard-compliant Fortran 90 and Fortran 77 programs.   It also supports
the ISO/IEC TR-15581 enhancements to allocatable arrays.

GNU Fortran also have a partial support for ISO/IEC 1539-1:2004
(Fortran 2003), ISO/IEC 1539-1:2010 (Fortran 2008), the Technical
Specification ``Further Interoperability of Fortran with C``
(ISO/IEC TS 29113:2012).  Full support of those standards and future
Fortran standards is planned.  The current status of the support is
can be found in the Fortran 2003 status, Fortran 2008
status and Fortran 2018 status sections of the documentation.

Additionally, the GNU Fortran compilers supports the OpenMP specification
(version 4.5 and partial support of the features of the 5.0 version,
http://openmp.org/openmp-specifications/).
There also is support for the OpenACC specification (targeting
version 2.6, http://www.openacc.org/).  See
https://gcc.gnu.org/wiki/OpenACC for more information.

.. _varying-length-character-strings:

Varying Length Character Strings
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Varying length character strings

.. index:: Varying length strings

.. index:: strings, varying length

The Fortran 95 standard specifies in Part 2 (ISO/IEC 1539-2:2000)
varying length character strings.  While GNU Fortran currently does not
support such strings directly, there exist two Fortran implementations
for them, which work with GNU Fortran.  They can be found at
http://www.fortran.com/iso_varying_string.f95 and at
ftp://ftp.nag.co.uk/sc22wg5/ISO_VARYING_STRING/.

Deferred-length character strings of Fortran 2003 supports part of
the features of ``ISO_VARYING_STRING`` and should be considered as
replacement. (Namely, allocatable or pointers of the type
``character(len=:)``.)

.. =====================================================================
   PART I: INVOCATION REFERENCE
   =====================================================================

.. -
   Compiler Options
   -

Only the most useful options are listed here; see below for the
remainder.

For instructions on reporting bugs, see
|bugurl|.

See the Info entry for :command:`gfortran` for contributors to GCC and
GNU Fortran.