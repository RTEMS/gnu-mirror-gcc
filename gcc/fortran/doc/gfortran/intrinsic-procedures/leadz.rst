..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _leadz:

LEADZ --- Number of leading zero bits of an integer
***************************************************

.. index:: LEADZ

.. index:: zero bits

.. function:: LEADZ

  ``LEADZ`` returns the number of leading zero bits of an integer.

  :param I:
    Shall be of type ``INTEGER``.

  :return:
    The type of the return value is the default ``INTEGER``.
    If all the bits of ``I`` are zero, the result value is ``BIT_SIZE(I)``.

  :samp:`{Standard}:`
    Fortran 2008 and later

  :samp:`{Class}:`
    Elemental function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    RESULT = LEADZ(I)

  :samp:`{Example}:`

    .. code-block:: fortran

      PROGRAM test_leadz
        WRITE (*,*) BIT_SIZE(1)  ! prints 32
        WRITE (*,*) LEADZ(1)     ! prints 31
      END PROGRAM

  :samp:`{See also}:`
    BIT_SIZE,
    TRAILZ,
    POPCNT,
    POPPAR