..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _ishft:

ISHFT --- Shift bits
********************

.. index:: ISHFT

.. index:: BSHFT

.. index:: IISHFT

.. index:: JISHFT

.. index:: KISHFT

.. index:: bits, shift

.. function:: ISHFT

  ``ISHFT`` returns a value corresponding to :samp:`{I}` with all of the
  bits shifted :samp:`{SHIFT}` places.  A value of :samp:`{SHIFT}` greater than
  zero corresponds to a left shift, a value of zero corresponds to no
  shift, and a value less than zero corresponds to a right shift.  If the
  absolute value of :samp:`{SHIFT}` is greater than ``BIT_SIZE(I)``, the
  value is undefined.  Bits shifted out from the left end or right end are
  lost; zeros are shifted in from the opposite end.

  :param I:
    The type shall be ``INTEGER``.

  :param SHIFT:
    The type shall be ``INTEGER``.

  :return:
    The return value is of type ``INTEGER`` and of the same kind as
    :samp:`{I}`.

  :samp:`{Standard}:`
    Fortran 90 and later, has overloads that are GNU extensions

  :samp:`{Class}:`
    Elemental function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    RESULT = ISHFT(I, SHIFT)

  :samp:`{Specific names}:`
    =============  ================  ==============  ====================
    Name           Argument          Return type     Standard
    =============  ================  ==============  ====================
    ``ISHFT(A)``   ``INTEGER A``     ``INTEGER``     Fortran 90 and later
    ``BSHFT(A)``   ``INTEGER(1) A``  ``INTEGER(1)``  GNU extension
    ``IISHFT(A)``  ``INTEGER(2) A``  ``INTEGER(2)``  GNU extension
    ``JISHFT(A)``  ``INTEGER(4) A``  ``INTEGER(4)``  GNU extension
    ``KISHFT(A)``  ``INTEGER(8) A``  ``INTEGER(8)``  GNU extension
    =============  ================  ==============  ====================

  :samp:`{See also}:`
    ISHFTC