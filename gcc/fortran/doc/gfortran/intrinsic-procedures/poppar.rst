..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _poppar:

POPPAR --- Parity of the number of bits set
*******************************************

.. index:: POPPAR

.. index:: binary representation

.. index:: parity

.. function:: POPPAR(I)

  ``POPPAR(I)`` returns parity of the integer ``I``, i.e. the parity
  of the number of bits set ('1' bits) in the binary representation of
  ``I``. It is equal to 0 if ``I`` has an even number of bits set,
  and 1 for an odd number of '1' bits.

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

    RESULT = POPPAR(I)

  :samp:`{Example}:`

    .. code-block:: fortran

      program test_population
        print *, popcnt(127),       poppar(127)
        print *, popcnt(huge(0_4)), poppar(huge(0_4))
        print *, popcnt(huge(0_8)), poppar(huge(0_8))
      end program test_population

  :samp:`{See also}:`
    POPCNT,
    LEADZ,
    TRAILZ