..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _iargc:

IARGC --- Get the number of command line arguments
**************************************************

.. index:: IARGC

.. index:: command-line arguments

.. index:: command-line arguments, number of

.. index:: arguments, to program

.. function:: IARGC

  ``IARGC`` returns the number of arguments passed on the
  command line when the containing program was invoked.

  :return:
    The number of command line arguments, type ``INTEGER(4)``.

  :samp:`{Standard}:`
    GNU extension

  :samp:`{Class}:`
    Function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    RESULT = IARGC()

  :samp:`{Arguments}:`
    None

  :samp:`{Example}:`
    See GETARG

  :samp:`{See also}:`
    GNU Fortran 77 compatibility subroutine:
    GETARG
    Fortran 2003 functions and subroutines:
    GET_COMMAND,
    GET_COMMAND_ARGUMENT,
    COMMAND_ARGUMENT_COUNT