..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _complex:

COMPLEX --- Complex conversion function
***************************************

.. index:: COMPLEX

.. index:: complex numbers, conversion to

.. index:: conversion, to complex

.. function:: COMPLEX(X, Y)

  ``COMPLEX(X, Y)`` returns a complex number where :samp:`{X}` is converted
  to the real component and :samp:`{Y}` is converted to the imaginary
  component.

  :param X:
    The type may be ``INTEGER`` or ``REAL``.

  :param Y:
    The type may be ``INTEGER`` or ``REAL``.

  :return:
    If :samp:`{X}` and :samp:`{Y}` are both of ``INTEGER`` type, then the return
    value is of default ``COMPLEX`` type.

  :samp:`{Standard}:`
    GNU extension

  :samp:`{Class}:`
    Elemental function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    RESULT = COMPLEX(X, Y)

  :samp:`{Example}:`

    .. code-block:: fortran

      program test_complex
          integer :: i = 42
          real :: x = 3.14
          print *, complex(i, x)
      end program test_complex

  :samp:`{See also}:`
    CMPLX