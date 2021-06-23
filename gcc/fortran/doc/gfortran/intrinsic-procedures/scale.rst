..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _scale:

SCALE --- Scale a real value
****************************

.. index:: SCALE

.. index:: real number, scale

.. index:: floating point, scale

.. function:: SCALE(X,I)

  ``SCALE(X,I)`` returns ``X * RADIX(X)**I``.

  :param X:
    The type of the argument shall be a ``REAL``.

  :param I:
    The type of the argument shall be a ``INTEGER``.

  :return:
    The return value is of the same type and kind as :samp:`{X}`.
    Its value is ``X * RADIX(X)**I``.

  :samp:`{Standard}:`
    Fortran 90 and later

  :samp:`{Class}:`
    Elemental function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    RESULT = SCALE(X, I)

  :samp:`{Example}:`

    .. code-block:: fortran

      program test_scale
        real :: x = 178.1387e-4
        integer :: i = 5
        print *, scale(x,i), x*radix(x)**i
      end program test_scale