..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _iachar:

IACHAR --- Code in ASCII collating sequence
********************************************

.. index:: IACHAR

.. index:: ASCII collating sequence

.. index:: collating sequence, ASCII

.. index:: conversion, to integer

.. function:: IACHAR(C)

  ``IACHAR(C)`` returns the code for the ASCII character
  in the first character position of ``C``.

  :param C:
    Shall be a scalar ``CHARACTER``, with ``INTENT(IN)``

  :param KIND:
    (Optional) An ``INTEGER`` initialization
    expression indicating the kind parameter of the result.

  :return:
    The return value is of type ``INTEGER`` and of kind :samp:`{KIND}`. If
    :samp:`{KIND}` is absent, the return value is of default integer kind.

  :samp:`{Standard}:`
    Fortran 95 and later, with :samp:`{KIND}` argument Fortran 2003 and later

  :samp:`{Class}:`
    Elemental function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    RESULT = IACHAR(C [, KIND])

  :samp:`{Example}:`

    .. code-block:: fortran

      program test_iachar
        integer i
        i = iachar(' ')
      end program test_iachar

  :samp:`{Note}:`
    See ICHAR for a discussion of converting between numerical values
    and formatted string representations.

  :samp:`{See also}:`
    ACHAR,
    CHAR,
    ICHAR