..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _epsilon:

EPSILON --- Epsilon function
****************************

.. index:: EPSILON

.. index:: model representation, epsilon

.. function:: EPSILON(X)

  ``EPSILON(X)`` returns the smallest number :samp:`{E}` of the same kind
  as :samp:`{X}` such that 1 + E > 1.

  :param X:
    The type shall be ``REAL``.

  :return:
    The return value is of same type as the argument.

  :samp:`{Standard}:`
    Fortran 90 and later

  :samp:`{Class}:`
    Inquiry function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    RESULT = EPSILON(X)

  :samp:`{Example}:`

    .. code-block:: fortran

      program test_epsilon
          real :: x = 3.143
          real(8) :: y = 2.33
          print *, EPSILON(x)
          print *, EPSILON(y)
      end program test_epsilon