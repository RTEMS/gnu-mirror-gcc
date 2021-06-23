..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _sind:

SIND --- Sine function, degrees
*******************************

.. index:: SIND

.. index:: DSIND

.. index:: CSIND

.. index:: ZSIND

.. index:: CDSIND

.. index:: trigonometric function, sine, degrees

.. index:: sine, degrees

.. function:: SIND(X)

  ``SIND(X)`` computes the sine of :samp:`{X}` in degrees.

  :param X:
    The type shall be ``REAL`` or
    ``COMPLEX``.

  :return:
    The return value has same type and kind as :samp:`{X}`, and its value is in degrees.

  :samp:`{Standard}:`
    GNU extension, enabled with :option:`-fdec-math`.

  :samp:`{Class}:`
    Elemental function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    RESULT = SIND(X)

  :samp:`{Example}:`

    .. code-block:: fortran

      program test_sind
        real :: x = 0.0
        x = sind(x)
      end program test_sind

  :samp:`{Specific names}:`
    =============  ================  ==============  =============
    Name           Argument          Return type     Standard
    =============  ================  ==============  =============
    ``SIND(X)``    ``REAL(4) X``     ``REAL(4)``     GNU extension
    ``DSIND(X)``   ``REAL(8) X``     ``REAL(8)``     GNU extension
    ``CSIND(X)``   ``COMPLEX(4) X``  ``COMPLEX(4)``  GNU extension
    ``ZSIND(X)``   ``COMPLEX(8) X``  ``COMPLEX(8)``  GNU extension
    ``CDSIND(X)``  ``COMPLEX(8) X``  ``COMPLEX(8)``  GNU extension
    =============  ================  ==============  =============

  :samp:`{See also}:`
    Inverse function:
    ASIND
    Radians function:
    SIN