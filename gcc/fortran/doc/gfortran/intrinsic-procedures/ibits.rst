..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _ibits:

IBITS --- Bit extraction
************************

.. index:: IBITS

.. index:: BBITS

.. index:: IIBITS

.. index:: JIBITS

.. index:: KIBITS

.. index:: bits, get

.. index:: bits, extract

.. function:: IBITS

  ``IBITS`` extracts a field of length :samp:`{LEN}` from :samp:`{I}`,
  starting from bit position :samp:`{POS}` and extending left for :samp:`{LEN}`
  bits.  The result is right-justified and the remaining bits are
  zeroed.  The value of ``POS+LEN`` must be less than or equal to the
  value ``BIT_SIZE(I)``.

  :param I:
    The type shall be ``INTEGER``.

  :param POS:
    The type shall be ``INTEGER``.

  :param LEN:
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

    RESULT = IBITS(I, POS, LEN)

  :samp:`{Specific names}:`
    =============  ================  ==============  ====================
    Name           Argument          Return type     Standard
    =============  ================  ==============  ====================
    ``IBITS(A)``   ``INTEGER A``     ``INTEGER``     Fortran 90 and later
    ``BBITS(A)``   ``INTEGER(1) A``  ``INTEGER(1)``  GNU extension
    ``IIBITS(A)``  ``INTEGER(2) A``  ``INTEGER(2)``  GNU extension
    ``JIBITS(A)``  ``INTEGER(4) A``  ``INTEGER(4)``  GNU extension
    ``KIBITS(A)``  ``INTEGER(8) A``  ``INTEGER(8)``  GNU extension
    =============  ================  ==============  ====================

  :samp:`{See also}:`
    BIT_SIZE,
    IBCLR,
    IBSET,
    IAND,
    IOR,
    IEOR