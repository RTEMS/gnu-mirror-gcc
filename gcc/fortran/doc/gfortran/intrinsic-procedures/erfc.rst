..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _erfc:

ERFC --- Error function
************************

.. index:: ERFC

.. index:: error function, complementary

.. function:: ERFC(X)

  ``ERFC(X)`` computes the complementary error function of :samp:`{X}`.

  :param X:
    The type shall be ``REAL``.

  :return:
    The return value is of type ``REAL`` and of the same kind as :samp:`{X}`.
    It lies in the range 0 \leq erfc (x) \leq 2 .

  :samp:`{Standard}:`
    Fortran 2008 and later

  :samp:`{Class}:`
    Elemental function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    RESULT = ERFC(X)

  :samp:`{Example}:`

    .. code-block:: fortran

      program test_erfc
        real(8) :: x = 0.17_8
        x = erfc(x)
      end program test_erfc

  :samp:`{Specific names}:`
    ============  =============  ===========  =============
    Name          Argument       Return type  Standard
    ============  =============  ===========  =============
    ``DERFC(X)``  ``REAL(8) X``  ``REAL(8)``  GNU extension
    ============  =============  ===========  =============