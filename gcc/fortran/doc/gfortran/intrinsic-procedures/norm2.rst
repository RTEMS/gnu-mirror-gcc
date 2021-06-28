..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _norm2:

NORM2 --- Euclidean vector norms
********************************

.. index:: NORM2

.. index:: Euclidean vector norm

.. index:: L2 vector norm

.. index:: norm, Euclidean

.. function:: NORM2

  Calculates the Euclidean vector norm (L_2 norm)
  of :samp:`{ARRAY}` along dimension :samp:`{DIM}`.

  :param ARRAY:
    Shall be an array of type ``REAL``

  :param DIM:
    (Optional) shall be a scalar of type
    ``INTEGER`` with a value in the range from 1 to n, where n
    equals the rank of :samp:`{ARRAY}`.

  :return:
    The result is of the same type as :samp:`{ARRAY}`.

  :samp:`{Standard}:`
    Fortran 2008 and later

  :samp:`{Class}:`
    Transformational function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    RESULT = NORM2(ARRAY[, DIM])

  :samp:`{Example}:`

    .. code-block:: fortran

      PROGRAM test_sum
        REAL :: x(5) = [ real :: 1, 2, 3, 4, 5 ]
        print *, NORM2(x)  ! = sqrt(55.) ~ 7.416
      END PROGRAM