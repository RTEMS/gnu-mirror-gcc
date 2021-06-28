..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _parity:

PARITY --- Reduction with exclusive OR
**************************************

.. index:: PARITY

.. index:: Parity

.. index:: Reduction, XOR

.. index:: XOR reduction

.. function:: PARITY

  Calculates the parity, i.e. the reduction using ``.XOR.``,
  of :samp:`{MASK}` along dimension :samp:`{DIM}`.

  :param MASK:
    Shall be an array of type ``LOGICAL``

  :param DIM:
    (Optional) shall be a scalar of type
    ``INTEGER`` with a value in the range from 1 to n, where n
    equals the rank of :samp:`{MASK}`.

  :return:
    The result is of the same type as :samp:`{MASK}`.

  :samp:`{Standard}:`
    Fortran 2008 and later

  :samp:`{Class}:`
    Transformational function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    RESULT = PARITY(MASK[, DIM])

  :samp:`{Example}:`

    .. code-block:: fortran

      PROGRAM test_sum
        LOGICAL :: x(2) = [ .true., .false. ]
        print *, PARITY(x) ! prints "T" (true).
      END PROGRAM