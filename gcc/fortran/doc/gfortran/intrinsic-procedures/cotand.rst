..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _cotand:

COTAND --- Cotangent function, degrees
**************************************

.. index:: COTAND

.. index:: DCOTAND

.. index:: trigonometric function, cotangent, degrees

.. index:: cotangent, degrees

.. function:: COTAND(X)

  ``COTAND(X)`` computes the cotangent of :samp:`{X}` in degrees.  Equivalent to
  ``COSD(x)`` divided by ``SIND(x)``, or ``1 / TAND(x)``.

  :param X:
    The type shall be ``REAL`` or ``COMPLEX``.

  :return:
    The return value has same type and kind as :samp:`{X}`, and its value is in degrees.

  :samp:`{Standard}:`
    GNU extension, enabled with :option:`-fdec-math`.

    This function is for compatibility only and should be avoided in favor of
    standard constructs wherever possible.

  :samp:`{Class}:`
    Elemental function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    RESULT = COTAND(X)

  :samp:`{Example}:`

    .. code-block:: fortran

      program test_cotand
        real(8) :: x = 0.165_8
        x = cotand(x)
      end program test_cotand

  :samp:`{Specific names}:`
    ==============  =============  ===========  =============
    Name            Argument       Return type  Standard
    ==============  =============  ===========  =============
    ``COTAND(X)``   ``REAL(4) X``  ``REAL(4)``  GNU extension
    ``DCOTAND(X)``  ``REAL(8) X``  ``REAL(8)``  GNU extension
    ==============  =============  ===========  =============

  :samp:`{See also}:`
    Converse function:
    TAND
    Radians function:
    COTAN