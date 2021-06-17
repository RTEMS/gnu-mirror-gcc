..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _matmul:

MATMUL --- matrix multiplication
********************************

.. index:: MATMUL

.. index:: matrix multiplication

.. index:: product, matrix

.. function:: MATMUL

  Performs a matrix multiplication on numeric or logical arguments.

  :param MATRIX_A:
    An array of ``INTEGER``,
    ``REAL``, ``COMPLEX``, or ``LOGICAL`` type, with a rank of
    one or two.

  :param MATRIX_B:
    An array of ``INTEGER``,
    ``REAL``, or ``COMPLEX`` type if :samp:`{MATRIX_A}` is of a numeric
    type; otherwise, an array of ``LOGICAL`` type. The rank shall be one
    or two, and the first (or only) dimension of :samp:`{MATRIX_B}` shall be
    equal to the last (or only) dimension of :samp:`{MATRIX_A}`.
    :samp:`{MATRIX_A}` and :samp:`{MATRIX_B}` shall not both be rank one arrays.

  :return:
    The matrix product of :samp:`{MATRIX_A}` and :samp:`{MATRIX_B}`.  The type and
    kind of the result follow the usual type and kind promotion rules, as
    for the ``*`` or ``.AND.`` operators.

  :samp:`{Standard}:`
    Fortran 90 and later

  :samp:`{Class}:`
    Transformational function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    RESULT = MATMUL(MATRIX_A, MATRIX_B)