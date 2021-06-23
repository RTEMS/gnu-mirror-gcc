..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _int2:

INT2 --- Convert to 16-bit integer type
***************************************

.. index:: INT2

.. index:: SHORT

.. index:: conversion, to integer

.. function:: INT2

  Convert to a ``KIND=2`` integer type. This is equivalent to the
  standard ``INT`` intrinsic with an optional argument of
  ``KIND=2``, and is only included for backwards compatibility.

  :param A:
    Shall be of type ``INTEGER``,
    ``REAL``, or ``COMPLEX``.

  :return:
    The return value is a ``INTEGER(2)`` variable.

  :samp:`{Standard}:`
    GNU extension

  :samp:`{Class}:`
    Elemental function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    RESULT = INT2(A)

  :samp:`{See also}:`
    INT,
    INT8,
    LONG