..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _sin:

SIN --- Sine function
**********************

.. index:: SIN

.. index:: DSIN

.. index:: CSIN

.. index:: ZSIN

.. index:: CDSIN

.. index:: trigonometric function, sine

.. index:: sine

.. function:: SIN(X)

  ``SIN(X)`` computes the sine of :samp:`{X}`.

  :param X:
    The type shall be ``REAL`` or
    ``COMPLEX``.

  :return:
    The return value has same type and kind as :samp:`{X}`.

  :samp:`{Standard}:`
    Fortran 77 and later

  :samp:`{Class}:`
    Elemental function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    RESULT = SIN(X)

  :samp:`{Example}:`

    .. code-block:: fortran

      program test_sin
        real :: x = 0.0
        x = sin(x)
      end program test_sin

  :samp:`{Specific names}:`
    ============  ================  ==============  ====================
    Name          Argument          Return type     Standard
    ============  ================  ==============  ====================
    ``SIN(X)``    ``REAL(4) X``     ``REAL(4)``     Fortran 77 and later
    ``DSIN(X)``   ``REAL(8) X``     ``REAL(8)``     Fortran 77 and later
    ``CSIN(X)``   ``COMPLEX(4) X``  ``COMPLEX(4)``  Fortran 77 and later
    ``ZSIN(X)``   ``COMPLEX(8) X``  ``COMPLEX(8)``  GNU extension
    ``CDSIN(X)``  ``COMPLEX(8) X``  ``COMPLEX(8)``  GNU extension
    ============  ================  ==============  ====================

  :samp:`{See also}:`
    Inverse function:
    ASIN
    Degrees function:
    SIND