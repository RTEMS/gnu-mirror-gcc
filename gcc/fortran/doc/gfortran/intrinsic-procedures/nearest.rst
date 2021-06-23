..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _nearest:

NEAREST --- Nearest representable number
****************************************

.. index:: NEAREST

.. index:: real number, nearest different

.. index:: floating point, nearest different

.. function:: NEAREST(X, S)

  ``NEAREST(X, S)`` returns the processor-representable number nearest
  to ``X`` in the direction indicated by the sign of ``S``.

  :param X:
    Shall be of type ``REAL``.

  :param S:
    Shall be of type ``REAL`` and
    not equal to zero.

  :return:
    The return value is of the same type as ``X``. If ``S`` is
    positive, ``NEAREST`` returns the processor-representable number
    greater than ``X`` and nearest to it. If ``S`` is negative,
    ``NEAREST`` returns the processor-representable number smaller than
    ``X`` and nearest to it.

  :samp:`{Standard}:`
    Fortran 90 and later

  :samp:`{Class}:`
    Elemental function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    RESULT = NEAREST(X, S)

  :samp:`{Example}:`

    .. code-block:: fortran

      program test_nearest
        real :: x, y
        x = nearest(42.0, 1.0)
        y = nearest(42.0, -1.0)
        write (*,"(3(G20.15))") x, y, x - y
      end program test_nearest