..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _log10:

LOG10 --- Base 10 logarithm function
************************************

.. index:: LOG10

.. index:: ALOG10

.. index:: DLOG10

.. index:: exponential function, inverse

.. index:: logarithm function with base 10

.. index:: base 10 logarithm function

.. function:: LOG10(X)

  ``LOG10(X)`` computes the base 10 logarithm of :samp:`{X}`.

  :param X:
    The type shall be ``REAL``.

  :return:
    The return value is of type ``REAL`` or ``COMPLEX``.
    The kind type parameter is the same as :samp:`{X}`.

  :samp:`{Standard}:`
    Fortran 77 and later

  :samp:`{Class}:`
    Elemental function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    RESULT = LOG10(X)

  :samp:`{Example}:`

    .. code-block:: fortran

      program test_log10
        real(8) :: x = 10.0_8
        x = log10(x)
      end program test_log10

  :samp:`{Specific names}:`
    =============  =============  ===========  ====================
    Name           Argument       Return type  Standard
    =============  =============  ===========  ====================
    ``ALOG10(X)``  ``REAL(4) X``  ``REAL(4)``  Fortran 77 and later
    ``DLOG10(X)``  ``REAL(8) X``  ``REAL(8)``  Fortran 77 and later
    =============  =============  ===========  ====================