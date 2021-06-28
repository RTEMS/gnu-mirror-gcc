..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _iparity:

IPARITY --- Bitwise XOR of array elements
*****************************************

.. index:: IPARITY

.. index:: array, parity

.. index:: array, XOR

.. index:: bits, XOR of array elements

.. function:: IPARITY

  Reduces with bitwise XOR (exclusive or) the elements of :samp:`{ARRAY}` along
  dimension :samp:`{DIM}` if the corresponding element in :samp:`{MASK}` is ``TRUE``.

  :param ARRAY:
    Shall be an array of type ``INTEGER``

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
    Fortran 2008 and later

  :samp:`{Class}:`
    Transformational function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    RESULT = IPARITY(ARRAY[, MASK])
    RESULT = IPARITY(ARRAY, DIM[, MASK])

  :samp:`{Example}:`

    .. code-block:: fortran

      PROGRAM test_iparity
        INTEGER(1) :: a(2)

        a(1) = int(b'00100100', 1)
        a(2) = int(b'01101010', 1)

        ! prints 01001110
        PRINT '(b8.8)', IPARITY(a)
      END PROGRAM

  :samp:`{See also}:`
    IANY,
    IALL,
    IEOR,
    PARITY