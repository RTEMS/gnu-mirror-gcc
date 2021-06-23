..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _introduction-to-intrinsics:

Introduction to intrinsic procedures
************************************

The intrinsic procedures provided by GNU Fortran include all of the
intrinsic procedures required by the Fortran 95 standard, a set of
intrinsic procedures for backwards compatibility with G77, and a
selection of intrinsic procedures from the Fortran 2003 and Fortran 2008
standards.  Any conflict between a description here and a description in
either the Fortran 95 standard, the Fortran 2003 standard or the Fortran
2008 standard is unintentional, and the standard(s) should be considered
authoritative.

The enumeration of the ``KIND`` type parameter is processor defined in
the Fortran 95 standard.  GNU Fortran defines the default integer type and
default real type by ``INTEGER(KIND=4)`` and ``REAL(KIND=4)``,
respectively.  The standard mandates that both data types shall have
another kind, which have more precision.  On typical target architectures
supported by :command:`gfortran`, this kind type parameter is ``KIND=8``.
Hence, ``REAL(KIND=8)`` and ``DOUBLE PRECISION`` are equivalent.
In the description of generic intrinsic procedures, the kind type parameter
will be specified by ``KIND=*``, and in the description of specific
names for an intrinsic procedure the kind type parameter will be explicitly
given (e.g., ``REAL(KIND=4)`` or ``REAL(KIND=8)`` ).  Finally, for
brevity the optional ``KIND=`` syntax will be omitted.

Many of the intrinsic procedures take one or more optional arguments.
This document follows the convention used in the Fortran 95 standard,
and denotes such arguments by square brackets.

GNU Fortran offers the :option:`-std`:samp:`=f95` and :option:`-std`:samp:`=gnu` options,
which can be used to restrict the set of intrinsic procedures to a
given standard.  By default, :command:`gfortran` sets the :option:`-std`:samp:`=gnu`
option, and so all intrinsic procedures described here are accepted.  There
is one caveat.  For a select group of intrinsic procedures, :command:`g77`
implemented both a function and a subroutine.  Both classes
have been implemented in :command:`gfortran` for backwards compatibility
with :command:`g77`.  It is noted here that these functions and subroutines
cannot be intermixed in a given subprogram.  In the descriptions that follow,
the applicable standard for each intrinsic procedure is noted.