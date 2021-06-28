..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _long:

LONG --- Convert to integer type
********************************

.. index:: LONG

.. index:: conversion, to integer

.. function:: LONG

  Convert to a ``KIND=4`` integer type, which is the same size as a C
  ``long`` integer.  This is equivalent to the standard ``INT``
  intrinsic with an optional argument of ``KIND=4``, and is only
  included for backwards compatibility.

  :param A:
    Shall be of type ``INTEGER``,
    ``REAL``, or ``COMPLEX``.

  :return:
    The return value is a ``INTEGER(4)`` variable.

  :samp:`{Standard}:`
    GNU extension

  :samp:`{Class}:`
    Elemental function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    RESULT = LONG(A)

  :samp:`{See also}:`
    INT,
    INT2,
    INT8