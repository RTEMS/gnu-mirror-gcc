..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _asinh:

ASINH --- Inverse hyperbolic sine function
******************************************

.. index:: ASINH

.. index:: DASINH

.. index:: area hyperbolic sine

.. index:: inverse hyperbolic sine

.. index:: hyperbolic function, sine, inverse

.. index:: sine, hyperbolic, inverse

.. function:: ASINH(X)

  ``ASINH(X)`` computes the inverse hyperbolic sine of :samp:`{X}`.

  :param X:
    The type shall be ``REAL`` or ``COMPLEX``.

  :return:
    The return value is of the same type and kind as  :samp:`{X}`. If :samp:`{X}` is
    complex, the imaginary part of the result is in radians and lies between
    -\pi/2 \leq \Im \asinh(x) \leq \pi/2.

  :samp:`{Standard}:`
    Fortran 2008 and later

  :samp:`{Class}:`
    Elemental function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    RESULT = ASINH(X)

  :samp:`{Example}:`

    .. code-block:: fortran

      PROGRAM test_asinh
        REAL(8), DIMENSION(3) :: x = (/ -1.0, 0.0, 1.0 /)
        WRITE (*,*) ASINH(x)
      END PROGRAM

  :samp:`{Specific names}:`
    =============  =============  ===========  ==============
    Name           Argument       Return type  Standard
    =============  =============  ===========  ==============
    ``DASINH(X)``  ``REAL(8) X``  ``REAL(8)``  GNU extension.
    =============  =============  ===========  ==============

  :samp:`{See also}:`
    Inverse function:
    SINH