..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _precision:

PRECISION --- Decimal precision of a real kind
**********************************************

.. index:: PRECISION

.. index:: model representation, precision

.. function:: PRECISION(X)

  ``PRECISION(X)`` returns the decimal precision in the model of the
  type of ``X``.

  :param X:
    Shall be of type ``REAL`` or ``COMPLEX``. It may
    be scalar or valued.

  :return:
    The return value is of type ``INTEGER`` and of the default integer
    kind.

  :samp:`{Standard}:`
    Fortran 90 and later

  :samp:`{Class}:`
    Inquiry function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    RESULT = PRECISION(X)

  :samp:`{Example}:`

    .. code-block:: fortran

      program prec_and_range
        real(kind=4) :: x(2)
        complex(kind=8) :: y

        print *, precision(x), range(x)
        print *, precision(y), range(y)
      end program prec_and_range

  :samp:`{See also}:`
    SELECTED_REAL_KIND,
    RANGE