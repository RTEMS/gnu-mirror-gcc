..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _trailz:

TRAILZ --- Number of trailing zero bits of an integer
*****************************************************

.. index:: TRAILZ

.. index:: zero bits

.. function:: TRAILZ

  ``TRAILZ`` returns the number of trailing zero bits of an integer.

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

    RESULT = TRAILZ(I)

  :samp:`{Example}:`

    .. code-block:: fortran

      PROGRAM test_trailz
        WRITE (*,*) TRAILZ(8)  ! prints 3
      END PROGRAM

  :samp:`{See also}:`
    BIT_SIZE,
    LEADZ,
    POPPAR,
    POPCNT