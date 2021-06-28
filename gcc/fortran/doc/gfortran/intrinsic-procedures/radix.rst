..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _radix:

RADIX --- Base of a model number
********************************

.. index:: RADIX

.. index:: model representation, base

.. index:: model representation, radix

.. function:: RADIX(X)

  ``RADIX(X)`` returns the base of the model representing the entity :samp:`{X}`.

  :param X:
    Shall be of type ``INTEGER`` or ``REAL``

  :return:
    The return value is a scalar of type ``INTEGER`` and of the default
    integer kind.

  :samp:`{Standard}:`
    Fortran 90 and later

  :samp:`{Class}:`
    Inquiry function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    RESULT = RADIX(X)

  :samp:`{Example}:`

    .. code-block:: fortran

      program test_radix
        print *, "The radix for the default integer kind is", radix(0)
        print *, "The radix for the default real kind is", radix(0.0)
      end program test_radix

  :samp:`{See also}:`
    SELECTED_REAL_KIND