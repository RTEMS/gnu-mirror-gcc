..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _preprocessing-and-conditional-compilation:

Preprocessing and conditional compilation
*****************************************

.. index:: CPP

.. index:: FPP

.. index:: Conditional compilation

.. index:: Preprocessing

.. index:: preprocessor, include file handling

Many Fortran compilers including GNU Fortran allow passing the source code
through a C preprocessor (CPP; sometimes also called the Fortran preprocessor,
FPP) to allow for conditional compilation.  In the case of GNU Fortran,
this is the GNU C Preprocessor in the traditional mode.  On systems with
case-preserving file names, the preprocessor is automatically invoked if the
filename extension is :samp:`.F`, :samp:`.FOR`, :samp:`.FTN`, :samp:`.fpp`,
:samp:`.FPP`, :samp:`.F90`, :samp:`.F95`, :samp:`.F03` or :samp:`.F08`.  To manually
invoke the preprocessor on any file, use :option:`-cpp`, to disable
preprocessing on files where the preprocessor is run automatically, use
:option:`-nocpp`.

If a preprocessed file includes another file with the Fortran ``INCLUDE``
statement, the included file is not preprocessed.  To preprocess included
files, use the equivalent preprocessor statement ``#include``.

If GNU Fortran invokes the preprocessor, ``__GFORTRAN__``
is defined.  The macros ``__GNUC__``, ``__GNUC_MINOR__`` and
``__GNUC_PATCHLEVEL__`` can be used to determine the version of the
compiler.  See TopOverviewcppThe C Preprocessor for details.

GNU Fortran supports a number of ``INTEGER`` and ``REAL`` kind types
in additional to the kind types required by the Fortran standard.
The availability of any given kind type is architecture dependent.  The
following pre-defined preprocessor macros can be used to conditionally
include code for these additional kind types: ``__GFC_INT_1__``,
``__GFC_INT_2__``, ``__GFC_INT_8__``, ``__GFC_INT_16__``,
``__GFC_REAL_10__``, and ``__GFC_REAL_16__``.

While CPP is the de-facto standard for preprocessing Fortran code,
Part 3 of the Fortran 95 standard (ISO/IEC 1539-3:1998) defines
Conditional Compilation, which is not widely used and not directly
supported by the GNU Fortran compiler.  You can use the program coco
to preprocess such files (http://www.daniellnagle.com/coco.html).

.. -
   GNU Fortran and G77
   -