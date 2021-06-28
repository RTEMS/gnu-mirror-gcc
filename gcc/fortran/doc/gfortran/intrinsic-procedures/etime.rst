..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _etime:

ETIME --- Execution time subroutine (or function)
*************************************************

.. index:: ETIME

.. index:: time, elapsed

.. function:: ETIME(VALUES, TIME)

  ``ETIME(VALUES, TIME)`` returns the number of seconds of runtime
  since the start of the process's execution in :samp:`{TIME}`.  :samp:`{VALUES}`
  returns the user and system components of this time in ``VALUES(1)`` and
  ``VALUES(2)`` respectively. :samp:`{TIME}` is equal to ``VALUES(1) + VALUES(2)``.

  :param VALUES:
    The type shall be ``REAL(4), DIMENSION(2)``.

  :param TIME:
    The type shall be ``REAL(4)``.

  :return:
    Elapsed time in seconds since the start of program execution.

  :samp:`{Standard}:`
    GNU extension

  :samp:`{Class}:`
    Subroutine, function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    CALL ETIME(VALUES, TIME).
    TIME = ETIME(VALUES), (not recommended).

  :samp:`{Example}:`

    .. code-block:: fortran

      program test_etime
          integer(8) :: i, j
          real, dimension(2) :: tarray
          real :: result
          call ETIME(tarray, result)
          print *, result
          print *, tarray(1)
          print *, tarray(2)
          do i=1,100000000    ! Just a delay
              j = i * i - i
          end do
          call ETIME(tarray, result)
          print *, result
          print *, tarray(1)
          print *, tarray(2)
      end program test_etime

  :samp:`{See also}:`
    CPU_TIME