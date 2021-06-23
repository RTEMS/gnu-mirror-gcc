..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _digits:

DIGITS --- Significant binary digits function
*********************************************

.. index:: DIGITS

.. index:: model representation, significant digits

.. function:: DIGITS(X)

  ``DIGITS(X)`` returns the number of significant binary digits of the internal
  model representation of :samp:`{X}`.  For example, on a system using a 32-bit
  floating point representation, a default real number would likely return 24.

  :param X:
    The type may be ``INTEGER`` or ``REAL``.

  :return:
    The return value is of type ``INTEGER``.

  :samp:`{Standard}:`
    Fortran 90 and later

  :samp:`{Class}:`
    Inquiry function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    RESULT = DIGITS(X)

  :samp:`{Example}:`

    .. code-block:: fortran

      program test_digits
          integer :: i = 12345
          real :: x = 3.143
          real(8) :: y = 2.33
          print *, digits(i)
          print *, digits(x)
          print *, digits(y)
      end program test_digits