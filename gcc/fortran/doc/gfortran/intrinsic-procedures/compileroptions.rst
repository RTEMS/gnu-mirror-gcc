..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _compiler_options:

COMPILER_OPTIONS --- Options passed to the compiler
***************************************************

.. index:: COMPILER_OPTIONS

.. index:: flags inquiry function

.. index:: options inquiry function

.. index:: compiler flags inquiry function

.. function:: COMPILER_OPTIONS

  ``COMPILER_OPTIONS`` returns a string with the options used for
  compiling.

  :return:
    The return value is a default-kind string with system-dependent length.
    It contains the compiler flags used to compile the file, which called
    the ``COMPILER_OPTIONS`` intrinsic.

  :samp:`{Standard}:`
    Fortran 2008

  :samp:`{Class}:`
    Inquiry function of the module ``ISO_FORTRAN_ENV``

  :samp:`{Syntax}:`

  .. code-block:: fortran

    STR = COMPILER_OPTIONS()

  :samp:`{Arguments}:`
    None

  :samp:`{Example}:`

    .. code-block:: fortran

         use iso_fortran_env
         print '(4a)', 'This file was compiled by ', &
                       compiler_version(), ' using the options ', &
                       compiler_options()
         end

  :samp:`{See also}:`
    COMPILER_VERSION,
    ISO_FORTRAN_ENV