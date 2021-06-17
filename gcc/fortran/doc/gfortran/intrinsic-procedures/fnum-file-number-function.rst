..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _fnum:

FNUM --- File number function
*****************************

.. index:: FNUM

.. index:: file operation, file number

.. function:: FNUM(UNIT)

  ``FNUM(UNIT)`` returns the POSIX file descriptor number corresponding to the
  open Fortran I/O unit ``UNIT``.

  :param UNIT:
    The type shall be ``INTEGER``.

  :return:
    The return value is of type ``INTEGER``

  :samp:`{Standard}:`
    GNU extension

  :samp:`{Class}:`
    Function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    RESULT = FNUM(UNIT)

  :samp:`{Example}:`

    .. code-block:: fortran

      program test_fnum
        integer :: i
        open (unit=10, status = "scratch")
        i = fnum(10)
        print *, i
        close (10)
      end program test_fnum