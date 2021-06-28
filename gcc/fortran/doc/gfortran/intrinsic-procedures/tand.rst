..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _tand:

TAND --- Tangent function, degrees
**********************************

.. index:: TAND

.. index:: DTAND

.. index:: trigonometric function, tangent, degrees

.. index:: tangent, degrees

.. function:: TAND(X)

  ``TAND(X)`` computes the tangent of :samp:`{X}` in degrees.

  :param X:
    The type shall be ``REAL`` or ``COMPLEX``.

  :return:
    The return value has same type and kind as :samp:`{X}`, and its value is in degrees.

  :samp:`{Standard}:`
    GNU extension, enabled with :option:`-fdec-math`.

  :samp:`{Class}:`
    Elemental function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    RESULT = TAND(X)

  :samp:`{Example}:`

    .. code-block:: fortran

      program test_tand
        real(8) :: x = 0.165_8
        x = tand(x)
      end program test_tand

  :samp:`{Specific names}:`
    ============  =============  ===========  =============
    Name          Argument       Return type  Standard
    ============  =============  ===========  =============
    ``TAND(X)``   ``REAL(4) X``  ``REAL(4)``  GNU extension
    ``DTAND(X)``  ``REAL(8) X``  ``REAL(8)``  GNU extension
    ============  =============  ===========  =============

  :samp:`{See also}:`
    Inverse function:
    ATAND
    Radians function:
    TAN