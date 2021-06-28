..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _erf:

ERF --- Error function
***********************

.. index:: ERF

.. index:: error function

.. function:: ERF(X)

  ``ERF(X)`` computes the error function of :samp:`{X}`.

  :param X:
    The type shall be ``REAL``.

  :return:
    The return value is of type ``REAL``, of the same kind as
    :samp:`{X}` and lies in the range -1 \leq erf (x) \leq 1 .

  :samp:`{Standard}:`
    Fortran 2008 and later

  :samp:`{Class}:`
    Elemental function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    RESULT = ERF(X)

  :samp:`{Example}:`

    .. code-block:: fortran

      program test_erf
        real(8) :: x = 0.17_8
        x = erf(x)
      end program test_erf

  :samp:`{Specific names}:`
    ===========  =============  ===========  =============
    Name         Argument       Return type  Standard
    ===========  =============  ===========  =============
    ``DERF(X)``  ``REAL(8) X``  ``REAL(8)``  GNU extension
    ===========  =============  ===========  =============