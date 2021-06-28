..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _sum:

SUM --- Sum of array elements
*****************************

.. index:: SUM

.. index:: array, sum

.. index:: array, add elements

.. index:: array, conditionally add elements

.. index:: sum array elements

.. function:: SUM

  Adds the elements of :samp:`{ARRAY}` along dimension :samp:`{DIM}` if
  the corresponding element in :samp:`{MASK}` is ``TRUE``.

  :param ARRAY:
    Shall be an array of type ``INTEGER``,
    ``REAL`` or ``COMPLEX``.

  :param DIM:
    (Optional) shall be a scalar of type
    ``INTEGER`` with a value in the range from 1 to n, where n
    equals the rank of :samp:`{ARRAY}`.

  :param MASK:
    (Optional) shall be of type ``LOGICAL``
    and either be a scalar or an array of the same shape as :samp:`{ARRAY}`.

  :return:
    The result is of the same type as :samp:`{ARRAY}`.

  :samp:`{Standard}:`
    Fortran 90 and later

  :samp:`{Class}:`
    Transformational function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    RESULT = SUM(ARRAY[, MASK])
    RESULT = SUM(ARRAY, DIM[, MASK])

  :samp:`{Example}:`

    .. code-block:: fortran

      PROGRAM test_sum
        INTEGER :: x(5) = (/ 1, 2, 3, 4 ,5 /)
        print *, SUM(x)                        ! all elements, sum = 15
        print *, SUM(x, MASK=MOD(x, 2)==1)     ! odd elements, sum = 9
      END PROGRAM

  :samp:`{See also}:`
    PRODUCT