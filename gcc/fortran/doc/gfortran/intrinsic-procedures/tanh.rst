..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _tanh:

TANH --- Hyperbolic tangent function
*************************************

.. index:: TANH

.. index:: DTANH

.. index:: hyperbolic tangent

.. index:: hyperbolic function, tangent

.. index:: tangent, hyperbolic

.. function:: TANH(X)

  ``TANH(X)`` computes the hyperbolic tangent of :samp:`{X}`.

  :param X:
    The type shall be ``REAL`` or ``COMPLEX``.

  :return:
    The return value has same type and kind as :samp:`{X}`. If :samp:`{X}` is
    complex, the imaginary part of the result is in radians. If :samp:`{X}`
    is ``REAL``, the return value lies in the range
    - 1 \leq tanh(x) \leq 1 .

  :samp:`{Standard}:`
    Fortran 77 and later, for a complex argument Fortran 2008 or later

  :samp:`{Class}:`
    Elemental function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    X = TANH(X)

  :samp:`{Example}:`

    .. code-block:: fortran

      program test_tanh
        real(8) :: x = 2.1_8
        x = tanh(x)
      end program test_tanh

  :samp:`{Specific names}:`
    ============  =============  ===========  ====================
    Name          Argument       Return type  Standard
    ============  =============  ===========  ====================
    ``TANH(X)``   ``REAL(4) X``  ``REAL(4)``  Fortran 77 and later
    ``DTANH(X)``  ``REAL(8) X``  ``REAL(8)``  Fortran 77 and later
    ============  =============  ===========  ====================

  :samp:`{See also}:`
    ATANH