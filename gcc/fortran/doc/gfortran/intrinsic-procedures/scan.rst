..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _scan:

SCAN --- Scan a string for the presence of a set of characters
**************************************************************

.. index:: SCAN

.. index:: string, find subset

.. function:: SCAN

  Scans a :samp:`{STRING}` for any of the characters in a :samp:`{SET}`
  of characters.

  :param STRING:
    Shall be of type ``CHARACTER``.

  :param SET:
    Shall be of type ``CHARACTER``.

  :param BACK:
    (Optional) shall be of type ``LOGICAL``.

  :param KIND:
    (Optional) An ``INTEGER`` initialization
    expression indicating the kind parameter of the result.

  :return:
    The return value is of type ``INTEGER`` and of kind :samp:`{KIND}`. If
    :samp:`{KIND}` is absent, the return value is of default integer kind.

  :samp:`{Standard}:`
    Fortran 90 and later, with :samp:`{KIND}` argument Fortran 2003 and later

  :samp:`{Class}:`
    Elemental function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    RESULT = SCAN(STRING, SET[, BACK [, KIND]])

  :samp:`{Example}:`

    .. code-block:: fortran

      PROGRAM test_scan
        WRITE(*,*) SCAN("FORTRAN", "AO")          ! 2, found 'O'
        WRITE(*,*) SCAN("FORTRAN", "AO", .TRUE.)  ! 6, found 'A'
        WRITE(*,*) SCAN("FORTRAN", "C++")         ! 0, found none
      END PROGRAM

  :samp:`{See also}:`
    INDEX intrinsic,
    VERIFY