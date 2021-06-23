..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _sinh:

SINH --- Hyperbolic sine function
**********************************

.. index:: SINH

.. index:: DSINH

.. index:: hyperbolic sine

.. index:: hyperbolic function, sine

.. index:: sine, hyperbolic

.. function:: SINH(X)

  ``SINH(X)`` computes the hyperbolic sine of :samp:`{X}`.

  :param X:
    The type shall be ``REAL`` or ``COMPLEX``.

  :return:
    The return value has same type and kind as :samp:`{X}`.

  :samp:`{Standard}:`
    Fortran 90 and later, for a complex argument Fortran 2008 or later, has
    a GNU extension

  :samp:`{Class}:`
    Elemental function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    RESULT = SINH(X)

  :samp:`{Example}:`

    .. code-block:: fortran

      program test_sinh
        real(8) :: x = - 1.0_8
        x = sinh(x)
      end program test_sinh

  :samp:`{Specific names}:`
    ============  =============  ===========  ====================
    Name          Argument       Return type  Standard
    ============  =============  ===========  ====================
    ``DSINH(X)``  ``REAL(8) X``  ``REAL(8)``  Fortran 90 and later
    ============  =============  ===========  ====================

  :samp:`{See also}:`
    ASINH