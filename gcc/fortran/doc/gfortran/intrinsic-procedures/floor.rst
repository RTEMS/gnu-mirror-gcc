..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _floor:

FLOOR --- Integer floor function
********************************

.. index:: FLOOR

.. index:: floor

.. index:: rounding, floor

.. function:: FLOOR(A)

  ``FLOOR(A)`` returns the greatest integer less than or equal to :samp:`{X}`.

  :param A:
    The type shall be ``REAL``.

  :param KIND:
    (Optional) An ``INTEGER`` initialization
    expression indicating the kind parameter of the result.

  :return:
    The return value is of type ``INTEGER(KIND)`` if :samp:`{KIND}` is present
    and of default-kind ``INTEGER`` otherwise.

  :samp:`{Standard}:`
    Fortran 95 and later

  :samp:`{Class}:`
    Elemental function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    RESULT = FLOOR(A [, KIND])

  :samp:`{Example}:`

    .. code-block:: fortran

      program test_floor
          real :: x = 63.29
          real :: y = -63.59
          print *, floor(x) ! returns 63
          print *, floor(y) ! returns -64
      end program test_floor

  :samp:`{See also}:`
    CEILING,
    NINT