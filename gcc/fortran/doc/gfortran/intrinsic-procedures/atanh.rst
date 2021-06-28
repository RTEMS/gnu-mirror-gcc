..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _atanh:

ATANH --- Inverse hyperbolic tangent function
*********************************************

.. index:: ATANH

.. index:: DATANH

.. index:: area hyperbolic tangent

.. index:: inverse hyperbolic tangent

.. index:: hyperbolic function, tangent, inverse

.. index:: tangent, hyperbolic, inverse

.. function:: ATANH(X)

  ``ATANH(X)`` computes the inverse hyperbolic tangent of :samp:`{X}`.

  :param X:
    The type shall be ``REAL`` or ``COMPLEX``.

  :return:
    The return value has same type and kind as :samp:`{X}`. If :samp:`{X}` is
    complex, the imaginary part of the result is in radians and lies between
    -\pi/2 \leq \Im \atanh(x) \leq \pi/2.

  :samp:`{Standard}:`
    Fortran 2008 and later

  :samp:`{Class}:`
    Elemental function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    RESULT = ATANH(X)

  :samp:`{Example}:`

    .. code-block:: fortran

      PROGRAM test_atanh
        REAL, DIMENSION(3) :: x = (/ -1.0, 0.0, 1.0 /)
        WRITE (*,*) ATANH(x)
      END PROGRAM

  :samp:`{Specific names}:`
    =============  =============  ===========  =============
    Name           Argument       Return type  Standard
    =============  =============  ===========  =============
    ``DATANH(X)``  ``REAL(8) X``  ``REAL(8)``  GNU extension
    =============  =============  ===========  =============

  :samp:`{See also}:`
    Inverse function:
    TANH