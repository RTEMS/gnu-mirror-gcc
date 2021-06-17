..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _trim:

TRIM --- Remove trailing blank characters of a string
*****************************************************

.. index:: TRIM

.. index:: string, remove trailing whitespace

.. function:: TRIM

  Removes trailing blank characters of a string.

  :param STRING:
    Shall be a scalar of type ``CHARACTER``.

  :return:
    A scalar of type ``CHARACTER`` which length is that of :samp:`{STRING}`
    less the number of trailing blanks.

  :samp:`{Standard}:`
    Fortran 90 and later

  :samp:`{Class}:`
    Transformational function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    RESULT = TRIM(STRING)

  :samp:`{Example}:`

    .. code-block:: fortran

      PROGRAM test_trim
        CHARACTER(len=10), PARAMETER :: s = "GFORTRAN  "
        WRITE(*,*) LEN(s), LEN(TRIM(s))  ! "10 8", with/without trailing blanks
      END PROGRAM

  :samp:`{See also}:`
    ADJUSTL,
    ADJUSTR