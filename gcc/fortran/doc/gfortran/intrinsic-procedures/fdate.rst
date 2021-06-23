..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _fdate:

FDATE --- Get the current time as a string
******************************************

.. index:: FDATE

.. index:: time, current

.. index:: current time

.. index:: date, current

.. index:: current date

.. function:: FDATE(DATE)

  ``FDATE(DATE)`` returns the current date (using the same format as
  CTIME) in :samp:`{DATE}`. It is equivalent to ``CALL CTIME(DATE,
  TIME())``.

  :param DATE:
    The type shall be of type ``CHARACTER`` of the
    default kind. It is an ``INTENT(OUT)`` argument.  If the length of
    this variable is too short for the date and time string to fit
    completely, it will be blank on procedure return.

  :return:
    The current date and time as a string.

  :samp:`{Standard}:`
    GNU extension

  :samp:`{Class}:`
    Subroutine, function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    CALL FDATE(DATE).
    DATE = FDATE().

  :samp:`{Example}:`

    .. code-block:: fortran

      program test_fdate
          integer(8) :: i, j
          character(len=30) :: date
          call fdate(date)
          print *, 'Program started on ', date
          do i = 1, 100000000 ! Just a delay
              j = i * i - i
          end do
          call fdate(date)
          print *, 'Program ended on ', date
      end program test_fdate

  :samp:`{See also}:`
    DATE_AND_TIME,
    CTIME