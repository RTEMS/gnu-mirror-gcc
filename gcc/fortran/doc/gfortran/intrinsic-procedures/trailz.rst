..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _trailz:

TRAILZ --- Number of trailing zero bits of an integer
*****************************************************

.. index:: TRAILZ, zero bits

.. function:: TRAILZ(I)

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
    :ref:`BIT_SIZE`,
    :ref:`LEADZ`,
    :ref:`POPPAR`,
    :ref:`POPCNT`