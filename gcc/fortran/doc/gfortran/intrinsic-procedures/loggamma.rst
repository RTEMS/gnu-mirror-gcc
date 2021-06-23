..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _log_gamma:

LOG_GAMMA --- Logarithm of the Gamma function
*********************************************

.. index:: LOG_GAMMA

.. index:: LGAMMA

.. index:: ALGAMA

.. index:: DLGAMA

.. index:: Gamma function, logarithm of

.. function:: LOG_GAMMA(X)

  ``LOG_GAMMA(X)`` computes the natural logarithm of the absolute value
  of the Gamma (\Gamma) function.

  :param X:
    Shall be of type ``REAL`` and neither zero
    nor a negative integer.

  :return:
    The return value is of type ``REAL`` of the same kind as :samp:`{X}`.

  :samp:`{Standard}:`
    Fortran 2008 and later

  :samp:`{Class}:`
    Elemental function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    X = LOG_GAMMA(X)

  :samp:`{Example}:`

    .. code-block:: fortran

      program test_log_gamma
        real :: x = 1.0
        x = lgamma(x) ! returns 0.0
      end program test_log_gamma

  :samp:`{Specific names}:`
    =============  =============  ===========  =============
    Name           Argument       Return type  Standard
    =============  =============  ===========  =============
    ``LGAMMA(X)``  ``REAL(4) X``  ``REAL(4)``  GNU extension
    ``ALGAMA(X)``  ``REAL(4) X``  ``REAL(4)``  GNU extension
    ``DLGAMA(X)``  ``REAL(8) X``  ``REAL(8)``  GNU extension
    =============  =============  ===========  =============

  :samp:`{See also}:`
    Gamma function:
    GAMMA