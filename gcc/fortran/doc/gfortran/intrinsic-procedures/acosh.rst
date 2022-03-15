..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. index:: ACOSH, DACOSH, area hyperbolic cosine, inverse hyperbolic cosine, hyperbolic function, cosine, inverse, cosine, hyperbolic, inverse

.. _acosh:

ACOSH --- Inverse hyperbolic cosine function
********************************************

.. function:: ACOSH(X)

  ``ACOSH(X)`` computes the inverse hyperbolic cosine of :samp:`{X}`.

  :param X:
    The type shall be ``REAL`` or ``COMPLEX``.

  :return:
    The return value has the same type and kind as :samp:`{X}`. If :samp:`{X}` is
    complex, the imaginary part of the result is in radians and lies between
    0 \leq \Im \acosh(x) \leq \pi.

  :samp:`{Standard}:`
    Fortran 2008 and later

  :samp:`{Class}:`
    Elemental function

  :samp:`{Syntax}:`

    .. code-block:: fortran

      RESULT = ACOSH(X)

  :samp:`{Example}:`

    .. code-block:: fortran

      PROGRAM test_acosh
        REAL(8), DIMENSION(3) :: x = (/ 1.0, 2.0, 3.0 /)
        WRITE (*,*) ACOSH(x)
      END PROGRAM

  :samp:`{Specific names}:`
    =============  =============  ===========  =============
    Name           Argument       Return type  Standard
    =============  =============  ===========  =============
    ``DACOSH(X)``  ``REAL(8) X``  ``REAL(8)``  GNU extension
    =============  =============  ===========  =============

  :samp:`{See also}:`
    Inverse function:
    :ref:`COSH`