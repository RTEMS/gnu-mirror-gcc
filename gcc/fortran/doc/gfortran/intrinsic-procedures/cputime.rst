..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. index:: CPU_TIME, time, elapsed

.. _cpu_time:

CPU_TIME --- CPU elapsed time in seconds
****************************************

.. function:: CPU_TIME(TIME)

  Returns a ``REAL`` value representing the elapsed CPU time in
  seconds.  This is useful for testing segments of code to determine
  execution time.

  :param TIME:
    The type shall be ``REAL`` with ``INTENT(OUT)``.

  :return:
    None

  :samp:`{Standard}:`
    Fortran 95 and later

  :samp:`{Class}:`
    Subroutine

  :samp:`{Syntax}:`

    .. code-block:: fortran

      CALL CPU_TIME(TIME)

  :samp:`{Example}:`

    .. code-block:: fortran

      program test_cpu_time
          real :: start, finish
          call cpu_time(start)
              ! put code to test here
          call cpu_time(finish)
          print '("Time = ",f6.3," seconds.")',finish-start
      end program test_cpu_time

  :samp:`{See also}:`
    :ref:`SYSTEM_CLOCK`,
    :ref:`DATE_AND_TIME`