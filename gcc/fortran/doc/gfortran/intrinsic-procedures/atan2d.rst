..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _atan2d:

ATAN2D --- Arctangent function, degrees
***************************************

.. index:: ATAN2D

.. index:: DATAN2D

.. index:: trigonometric function, tangent, inverse, degrees

.. index:: tangent, inverse, degrees

.. function:: ATAN2D(Y, X)

  ``ATAN2D(Y, X)`` computes the principal value of the argument
  function of the complex number X + i Y in degrees.  This function can
  be used to transform from Cartesian into polar coordinates and
  allows to determine the angle in the correct quadrant.

  :param Y:
    The type shall be ``REAL``.

  :param X:
    The type and kind type parameter shall be the same as :samp:`{Y}`.
    If :samp:`{Y}` is zero, then :samp:`{X}` must be nonzero.

  :return:
    The return value has the same type and kind type parameter as :samp:`{Y}`. It
    is the principal value of the complex number X + i Y.  If :samp:`{X}`
    is nonzero, then it lies in the range -180 \le \atan (x) \leq 180.
    The sign is positive if :samp:`{Y}` is positive.  If :samp:`{Y}` is zero, then
    the return value is zero if :samp:`{X}` is strictly positive, 180 if
    :samp:`{X}` is negative and :samp:`{Y}` is positive zero (or the processor does
    not handle signed zeros), and -180 if :samp:`{X}` is negative and
    :samp:`{Y}` is negative zero.  Finally, if :samp:`{X}` is zero, then the
    magnitude of the result is 90.

  :samp:`{Standard}:`
    GNU extension, enabled with :option:`-fdec-math`.

  :samp:`{Class}:`
    Elemental function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    RESULT = ATAN2D(Y, X)

  :samp:`{Example}:`

    .. code-block:: fortran

      program test_atan2d
        real(4) :: x = 1.e0_4, y = 0.5e0_4
        x = atan2d(y,x)
      end program test_atan2d

  :samp:`{Specific names}:`
    =================  ================  ===========  =============
    Name               Argument          Return type  Standard
    =================  ================  ===========  =============
    ``ATAN2D(X, Y)``   ``REAL(4) X, Y``  ``REAL(4)``  GNU extension
    ``DATAN2D(X, Y)``  ``REAL(8) X, Y``  ``REAL(8)``  GNU extension
    =================  ================  ===========  =============

  :samp:`{See also}:`
    Alias:
    ATAND
    Radians function:
    ATAN2