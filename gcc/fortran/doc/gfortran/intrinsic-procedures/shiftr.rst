..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _shiftr:

SHIFTR --- Right shift
**********************

.. index:: SHIFTR

.. index:: bits, shift right

.. index:: shift, right

.. function:: SHIFTR

  ``SHIFTR`` returns a value corresponding to :samp:`{I}` with all of the
  bits shifted right by :samp:`{SHIFT}` places.  :samp:`{SHIFT}` shall be
  nonnegative and less than or equal to ``BIT_SIZE(I)``, otherwise
  the result value is undefined.  Bits shifted out from the right end
  are lost, and bits shifted in from the left end are set to 0.

  :param I:
    The type shall be ``INTEGER``.

  :param SHIFT:
    The type shall be ``INTEGER``.

  :return:
    The return value is of type ``INTEGER`` and of the same kind as
    :samp:`{I}`.

  :samp:`{Standard}:`
    Fortran 2008 and later

  :samp:`{Class}:`
    Elemental function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    RESULT = SHIFTR(I, SHIFT)

  :samp:`{See also}:`
    SHIFTA,
    SHIFTL