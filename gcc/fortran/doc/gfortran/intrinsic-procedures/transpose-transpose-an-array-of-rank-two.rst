..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _transpose:

TRANSPOSE --- Transpose an array of rank two
********************************************

.. index:: TRANSPOSE

.. index:: array, transpose

.. index:: matrix, transpose

.. index:: transpose

.. function:: TRANSPOSE

  Transpose an array of rank two. Element (i, j) of the result has the value
  ``MATRIX(j, i)``, for all i, j.

  :param MATRIX:
    Shall be an array of any type and have a rank of two.

  :return:
    The result has the same type as :samp:`{MATRIX}`, and has shape
    ``(/ m, n /)`` if :samp:`{MATRIX}` has shape ``(/ n, m /)``.

  :samp:`{Standard}:`
    Fortran 90 and later

  :samp:`{Class}:`
    Transformational function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    RESULT = TRANSPOSE(MATRIX)