..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _huge:

HUGE --- Largest number of a kind
*********************************

.. index:: HUGE

.. index:: limits, largest number

.. index:: model representation, largest number

.. function:: HUGE(X)

  ``HUGE(X)`` returns the largest number that is not an infinity in
  the model of the type of ``X``.

  :param X:
    Shall be of type ``REAL`` or ``INTEGER``.

  :return:
    The return value is of the same type and kind as :samp:`{X}`

  :samp:`{Standard}:`
    Fortran 90 and later

  :samp:`{Class}:`
    Inquiry function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    RESULT = HUGE(X)

  :samp:`{Example}:`

    .. code-block:: fortran

      program test_huge_tiny
        print *, huge(0), huge(0.0), huge(0.0d0)
        print *, tiny(0.0), tiny(0.0d0)
      end program test_huge_tiny