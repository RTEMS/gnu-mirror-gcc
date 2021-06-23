..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _adjustr:

ADJUSTR --- Right adjust a string
**********************************

.. index:: ADJUSTR

.. index:: string, adjust right

.. index:: adjust string

.. function:: ADJUSTR(STRING)

  ``ADJUSTR(STRING)`` will right adjust a string by removing trailing spaces.
  Spaces are inserted at the start of the string as needed.

  :param STR:
    The type shall be ``CHARACTER``.

  :return:
    The return value is of type ``CHARACTER`` and of the same kind as
    :samp:`{STRING}` where trailing spaces are removed and the same number of
    spaces are inserted at the start of :samp:`{STRING}`.

  :samp:`{Standard}:`
    Fortran 90 and later

  :samp:`{Class}:`
    Elemental function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    RESULT = ADJUSTR(STRING)

  :samp:`{Example}:`

    .. code-block:: fortran

      program test_adjustr
        character(len=20) :: str = 'gfortran'
        str = adjustr(str)
        print *, str
      end program test_adjustr

  :samp:`{See also}:`
    ADJUSTL,
    TRIM