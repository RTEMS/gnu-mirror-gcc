..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _popcnt:

POPCNT --- Number of bits set
*****************************

.. index:: POPCNT

.. index:: binary representation

.. index:: bits set

.. function:: POPCNT(I)

  ``POPCNT(I)`` returns the number of bits set ('1' bits) in the binary
  representation of ``I``.

  :param I:
    Shall be of type ``INTEGER``.

  :return:
    The return value is of type ``INTEGER`` and of the default integer
    kind.

  :samp:`{Standard}:`
    Fortran 2008 and later

  :samp:`{Class}:`
    Elemental function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    RESULT = POPCNT(I)

  :samp:`{Example}:`

    .. code-block:: fortran

      program test_population
        print *, popcnt(127),       poppar(127)
        print *, popcnt(huge(0_4)), poppar(huge(0_4))
        print *, popcnt(huge(0_8)), poppar(huge(0_8))
      end program test_population

  :samp:`{See also}:`
    POPPAR,
    LEADZ,
    TRAILZ