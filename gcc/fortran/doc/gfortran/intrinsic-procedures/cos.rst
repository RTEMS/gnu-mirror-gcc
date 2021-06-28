..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _cos:

COS --- Cosine function
***********************

.. index:: COS

.. index:: DCOS

.. index:: CCOS

.. index:: ZCOS

.. index:: CDCOS

.. index:: trigonometric function, cosine

.. index:: cosine

.. function:: COS(X)

  ``COS(X)`` computes the cosine of :samp:`{X}`.

  :param X:
    The type shall be ``REAL`` or
    ``COMPLEX``.

  :return:
    The return value is of the same type and kind as :samp:`{X}`. The real part
    of the result is in radians. If :samp:`{X}` is of the type ``REAL``,
    the return value lies in the range -1 \leq \cos (x) \leq 1.

  :samp:`{Standard}:`
    Fortran 77 and later, has overloads that are GNU extensions

  :samp:`{Class}:`
    Elemental function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    RESULT = COS(X)

  :samp:`{Example}:`

    .. code-block:: fortran

      program test_cos
        real :: x = 0.0
        x = cos(x)
      end program test_cos

  :samp:`{Specific names}:`
    ============  ================  ==============  ====================
    Name          Argument          Return type     Standard
    ============  ================  ==============  ====================
    ``COS(X)``    ``REAL(4) X``     ``REAL(4)``     Fortran 77 and later
    ``DCOS(X)``   ``REAL(8) X``     ``REAL(8)``     Fortran 77 and later
    ``CCOS(X)``   ``COMPLEX(4) X``  ``COMPLEX(4)``  Fortran 77 and later
    ``ZCOS(X)``   ``COMPLEX(8) X``  ``COMPLEX(8)``  GNU extension
    ``CDCOS(X)``  ``COMPLEX(8) X``  ``COMPLEX(8)``  GNU extension
    ============  ================  ==============  ====================

  :samp:`{See also}:`
    Inverse function:
    ACOS
    Degrees function:
    COSD