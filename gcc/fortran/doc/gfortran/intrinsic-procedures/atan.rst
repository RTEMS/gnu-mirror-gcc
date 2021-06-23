..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _atan:

ATAN --- Arctangent function
*****************************

.. index:: ATAN

.. index:: DATAN

.. index:: trigonometric function, tangent, inverse

.. index:: tangent, inverse

.. function:: ATAN(X)

  ``ATAN(X)`` computes the arctangent of :samp:`{X}`.

  :param X:
    The type shall be ``REAL`` or ``COMPLEX`` ;
    if :samp:`{Y}` is present, :samp:`{X}` shall be REAL.

  :param Y:
    The type and kind type parameter shall be the same as :samp:`{X}`.

  :return:
    The return value is of the same type and kind as :samp:`{X}`.
    If :samp:`{Y}` is present, the result is identical to ``ATAN2(Y,X)``.
    Otherwise, it the arcus tangent of :samp:`{X}`, where the real part of
    the result is in radians and lies in the range
    -\pi/2 \leq \Re \atan(x) \leq \pi/2.

  :samp:`{Standard}:`
    Fortran 77 and later, for a complex argument and for two arguments
    Fortran 2008 or later

  :samp:`{Class}:`
    Elemental function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    RESULT = ATAN(X)
    RESULT = ATAN(Y, X)

  :samp:`{Example}:`

    .. code-block:: fortran

      program test_atan
        real(8) :: x = 2.866_8
        x = atan(x)
      end program test_atan

  :samp:`{Specific names}:`
    ============  =============  ===========  ====================
    Name          Argument       Return type  Standard
    ============  =============  ===========  ====================
    ``ATAN(X)``   ``REAL(4) X``  ``REAL(4)``  Fortran 77 and later
    ``DATAN(X)``  ``REAL(8) X``  ``REAL(8)``  Fortran 77 and later
    ============  =============  ===========  ====================

  :samp:`{See also}:`
    Inverse function:
    TAN
    Degrees function:
    ATAND