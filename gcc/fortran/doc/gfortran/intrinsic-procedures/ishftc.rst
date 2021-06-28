..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _ishftc:

ISHFTC --- Shift bits circularly
********************************

.. index:: ISHFTC

.. index:: BSHFTC

.. index:: IISHFTC

.. index:: JISHFTC

.. index:: KISHFTC

.. index:: bits, shift circular

.. function:: ISHFTC

  ``ISHFTC`` returns a value corresponding to :samp:`{I}` with the
  rightmost :samp:`{SIZE}` bits shifted circularly :samp:`{SHIFT}` places; that
  is, bits shifted out one end are shifted into the opposite end.  A value
  of :samp:`{SHIFT}` greater than zero corresponds to a left shift, a value of
  zero corresponds to no shift, and a value less than zero corresponds to
  a right shift.  The absolute value of :samp:`{SHIFT}` must be less than
  :samp:`{SIZE}`.  If the :samp:`{SIZE}` argument is omitted, it is taken to be
  equivalent to ``BIT_SIZE(I)``.

  :param I:
    The type shall be ``INTEGER``.

  :param SHIFT:
    The type shall be ``INTEGER``.

  :param SIZE:
    (Optional) The type shall be ``INTEGER`` ;
    the value must be greater than zero and less than or equal to
    ``BIT_SIZE(I)``.

  :return:
    The return value is of type ``INTEGER`` and of the same kind as
    :samp:`{I}`.

  :samp:`{Standard}:`
    Fortran 90 and later, has overloads that are GNU extensions

  :samp:`{Class}:`
    Elemental function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    RESULT = ISHFTC(I, SHIFT [, SIZE])

  :samp:`{Specific names}:`
    ==============  ================  ==============  ====================
    Name            Argument          Return type     Standard
    ==============  ================  ==============  ====================
    ``ISHFTC(A)``   ``INTEGER A``     ``INTEGER``     Fortran 90 and later
    ``BSHFTC(A)``   ``INTEGER(1) A``  ``INTEGER(1)``  GNU extension
    ``IISHFTC(A)``  ``INTEGER(2) A``  ``INTEGER(2)``  GNU extension
    ``JISHFTC(A)``  ``INTEGER(4) A``  ``INTEGER(4)``  GNU extension
    ``KISHFTC(A)``  ``INTEGER(8) A``  ``INTEGER(8)``  GNU extension
    ==============  ================  ==============  ====================

  :samp:`{See also}:`
    ISHFT