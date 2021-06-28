..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _spacing:

SPACING --- Smallest distance between two numbers of a given type
*****************************************************************

.. index:: SPACING

.. index:: real number, relative spacing

.. index:: floating point, relative spacing

.. function:: SPACING

  Determines the distance between the argument :samp:`{X}` and the nearest
  adjacent number of the same type.

  :param X:
    Shall be of type ``REAL``.

  :return:
    The result is of the same type as the input argument :samp:`{X}`.

  :samp:`{Standard}:`
    Fortran 90 and later

  :samp:`{Class}:`
    Elemental function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    RESULT = SPACING(X)

  :samp:`{Example}:`

    .. code-block:: fortran

      PROGRAM test_spacing
        INTEGER, PARAMETER :: SGL = SELECTED_REAL_KIND(p=6, r=37)
        INTEGER, PARAMETER :: DBL = SELECTED_REAL_KIND(p=13, r=200)

        WRITE(*,*) spacing(1.0_SGL)      ! "1.1920929E-07"          on i686
        WRITE(*,*) spacing(1.0_DBL)      ! "2.220446049250313E-016" on i686
      END PROGRAM

  :samp:`{See also}:`
    RRSPACING