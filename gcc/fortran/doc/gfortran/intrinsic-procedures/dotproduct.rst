..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _dot_product:

DOT_PRODUCT --- Dot product function
************************************

.. index:: DOT_PRODUCT

.. index:: dot product

.. index:: vector product

.. index:: product, vector

.. function:: DOT_PRODUCT(VECTOR_A, VECTOR_B)

  ``DOT_PRODUCT(VECTOR_A, VECTOR_B)`` computes the dot product multiplication
  of two vectors :samp:`{VECTOR_A}` and :samp:`{VECTOR_B}`.  The two vectors may be
  either numeric or logical and must be arrays of rank one and of equal size. If
  the vectors are ``INTEGER`` or ``REAL``, the result is
  ``SUM(VECTOR_A*VECTOR_B)``. If the vectors are ``COMPLEX``, the result
  is ``SUM(CONJG(VECTOR_A)*VECTOR_B)``. If the vectors are ``LOGICAL``,
  the result is ``ANY(VECTOR_A .AND. VECTOR_B)``.

  :param VECTOR_A:
    The type shall be numeric or ``LOGICAL``, rank 1.

  :param VECTOR_B:
    The type shall be numeric if :samp:`{VECTOR_A}` is of numeric type or ``LOGICAL`` if :samp:`{VECTOR_A}` is of type ``LOGICAL``. :samp:`{VECTOR_B}` shall be a rank-one array.

  :return:
    If the arguments are numeric, the return value is a scalar of numeric type,
    ``INTEGER``, ``REAL``, or ``COMPLEX``.  If the arguments are
    ``LOGICAL``, the return value is ``.TRUE.`` or ``.FALSE.``.

  :samp:`{Standard}:`
    Fortran 90 and later

  :samp:`{Class}:`
    Transformational function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    RESULT = DOT_PRODUCT(VECTOR_A, VECTOR_B)

  :samp:`{Example}:`

    .. code-block:: fortran

      program test_dot_prod
          integer, dimension(3) :: a, b
          a = (/ 1, 2, 3 /)
          b = (/ 4, 5, 6 /)
          print '(3i3)', a
          print *
          print '(3i3)', b
          print *
          print *, dot_product(a,b)
      end program test_dot_prod