..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _bit_size:

BIT_SIZE --- Bit size inquiry function
**************************************

.. index:: BIT_SIZE

.. index:: bits, number of

.. index:: size of a variable, in bits

.. function:: BIT_SIZE(I)

  ``BIT_SIZE(I)`` returns the number of bits (integer precision plus sign bit)
  represented by the type of :samp:`{I}`.  The result of ``BIT_SIZE(I)`` is
  independent of the actual value of :samp:`{I}`.

  :param I:
    The type shall be ``INTEGER``.

  :return:
    The return value is of type ``INTEGER``

  :samp:`{Standard}:`
    Fortran 90 and later

  :samp:`{Class}:`
    Inquiry function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    RESULT = BIT_SIZE(I)

  :samp:`{Example}:`

    .. code-block:: fortran

      program test_bit_size
          integer :: i = 123
          integer :: size
          size = bit_size(i)
          print *, size
      end program test_bit_size