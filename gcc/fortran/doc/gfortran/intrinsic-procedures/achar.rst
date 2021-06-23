..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _achar:

ACHAR --- Character in ASCII collating sequence
************************************************

.. index:: ACHAR

.. index:: ASCII collating sequence

.. index:: collating sequence, ASCII

.. function:: ACHAR(I)

  ``ACHAR(I)`` returns the character located at position ``I``
  in the ASCII collating sequence.

  :param I:
    The type shall be ``INTEGER``.

  :param KIND:
    (Optional) An ``INTEGER`` initialization
    expression indicating the kind parameter of the result.

  :return:
    The return value is of type ``CHARACTER`` with a length of one.
    If the :samp:`{KIND}` argument is present, the return value is of the
    specified kind and of the default kind otherwise.

  :samp:`{Standard}:`
    Fortran 77 and later, with :samp:`{KIND}` argument Fortran 2003 and later

  :samp:`{Class}:`
    Elemental function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    RESULT = ACHAR(I [, KIND])

  :samp:`{Example}:`

    .. code-block:: fortran

      program test_achar
        character c
        c = achar(32)
      end program test_achar

  :samp:`{Note}:`
    See ICHAR for a discussion of converting between numerical values
    and formatted string representations.

  :samp:`{See also}:`
    CHAR,
    IACHAR,
    ICHAR