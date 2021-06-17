..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _logical:

LOGICAL --- Convert to logical type
***********************************

.. index:: LOGICAL

.. index:: conversion, to logical

.. function:: LOGICAL

  Converts one kind of ``LOGICAL`` variable to another.

  :param L:
    The type shall be ``LOGICAL``.

  :param KIND:
    (Optional) An ``INTEGER`` initialization
    expression indicating the kind parameter of the result.

  :return:
    The return value is a ``LOGICAL`` value equal to :samp:`{L}`, with a
    kind corresponding to :samp:`{KIND}`, or of the default logical kind if
    :samp:`{KIND}` is not given.

  :samp:`{Standard}:`
    Fortran 90 and later

  :samp:`{Class}:`
    Elemental function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    RESULT = LOGICAL(L [, KIND])

  :samp:`{See also}:`
    INT,
    REAL,
    CMPLX