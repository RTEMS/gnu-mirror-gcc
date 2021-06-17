..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _ceiling:

CEILING --- Integer ceiling function
************************************

.. index:: CEILING

.. index:: ceiling

.. index:: rounding, ceiling

.. function:: CEILING(A)

  ``CEILING(A)`` returns the least integer greater than or equal to :samp:`{A}`.

  :param A:
    The type shall be ``REAL``.

  :param KIND:
    (Optional) An ``INTEGER`` initialization
    expression indicating the kind parameter of the result.

  :return:
    The return value is of type ``INTEGER(KIND)`` if :samp:`{KIND}` is present
    and a default-kind ``INTEGER`` otherwise.

  :samp:`{Standard}:`
    Fortran 95 and later

  :samp:`{Class}:`
    Elemental function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    RESULT = CEILING(A [, KIND])

  :samp:`{Example}:`

    .. code-block:: fortran

      program test_ceiling
          real :: x = 63.29
          real :: y = -63.59
          print *, ceiling(x) ! returns 64
          print *, ceiling(y) ! returns -63
      end program test_ceiling

  :samp:`{See also}:`
    FLOOR,
    NINT