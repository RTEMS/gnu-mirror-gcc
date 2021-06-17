..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _tan:

TAN --- Tangent function
************************

.. index:: TAN

.. index:: DTAN

.. index:: trigonometric function, tangent

.. index:: tangent

.. function:: TAN(X)

  ``TAN(X)`` computes the tangent of :samp:`{X}`.

  :param X:
    The type shall be ``REAL`` or ``COMPLEX``.

  :return:
    The return value has same type and kind as :samp:`{X}`, and its value is in radians.

  :samp:`{Standard}:`
    Fortran 77 and later, for a complex argument Fortran 2008 or later

  :samp:`{Class}:`
    Elemental function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    RESULT = TAN(X)

  :samp:`{Example}:`

    .. code-block:: fortran

      program test_tan
        real(8) :: x = 0.165_8
        x = tan(x)
      end program test_tan

  :samp:`{Specific names}:`
    ===========  =============  ===========  ====================
    Name         Argument       Return type  Standard
    ===========  =============  ===========  ====================
    ``TAN(X)``   ``REAL(4) X``  ``REAL(4)``  Fortran 77 and later
    ``DTAN(X)``  ``REAL(8) X``  ``REAL(8)``  Fortran 77 and later
    ===========  =============  ===========  ====================

  :samp:`{See also}:`
    Inverse function:
    ATAN
    Degrees function:
    TAND