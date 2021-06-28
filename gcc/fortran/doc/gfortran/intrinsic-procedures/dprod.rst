..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _dprod:

DPROD --- Double product function
*********************************

.. index:: DPROD

.. index:: product, double-precision

.. function:: DPROD(X,Y)

  ``DPROD(X,Y)`` returns the product ``X*Y``.

  :param X:
    The type shall be ``REAL``.

  :param Y:
    The type shall be ``REAL``.

  :return:
    The return value is of type ``REAL(8)``.

  :samp:`{Standard}:`
    Fortran 77 and later

  :samp:`{Class}:`
    Elemental function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    RESULT = DPROD(X, Y)

  :samp:`{Example}:`

    .. code-block:: fortran

      program test_dprod
          real :: x = 5.2
          real :: y = 2.3
          real(8) :: d
          d = dprod(x,y)
          print *, d
      end program test_dprod

  :samp:`{Specific names}:`
    ==============  ================  ===========  ====================
    Name            Argument          Return type  Standard
    ==============  ================  ===========  ====================
    ``DPROD(X,Y)``  ``REAL(4) X, Y``  ``REAL(8)``  Fortran 77 and later
    ==============  ================  ===========  ====================