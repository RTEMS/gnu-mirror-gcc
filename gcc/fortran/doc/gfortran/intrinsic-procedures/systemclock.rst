..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _system_clock:

SYSTEM_CLOCK --- Time function
******************************

.. index:: SYSTEM_CLOCK

.. index:: time, clock ticks

.. index:: clock ticks

.. function:: SYSTEM_CLOCK

  Determines the :samp:`{COUNT}` of a processor clock since an unspecified
  time in the past modulo :samp:`{COUNT_MAX}`, :samp:`{COUNT_RATE}` determines
  the number of clock ticks per second.  If the platform supports a
  monotonic clock, that clock is used and can, depending on the platform
  clock implementation, provide up to nanosecond resolution.  If a
  monotonic clock is not available, the implementation falls back to a
  realtime clock.

  :param COUNT:
    (Optional) shall be a scalar of type
    ``INTEGER`` with ``INTENT(OUT)``.

  :param COUNT_RATE:
    (Optional) shall be a scalar of type
    ``INTEGER`` or ``REAL``, with ``INTENT(OUT)``.

  :param COUNT_MAX:
    (Optional) shall be a scalar of type
    ``INTEGER`` with ``INTENT(OUT)``.

  :samp:`{Standard}:`
    Fortran 90 and later

  :samp:`{Class}:`
    Subroutine

  :samp:`{Syntax}:`

  .. code-block:: fortran

    CALL SYSTEM_CLOCK([COUNT, COUNT_RATE, COUNT_MAX])

  :samp:`{Example}:`

    .. code-block:: fortran

      PROGRAM test_system_clock
        INTEGER :: count, count_rate, count_max
        CALL SYSTEM_CLOCK(count, count_rate, count_max)
        WRITE(*,*) count, count_rate, count_max
      END PROGRAM

  :samp:`{See also}:`
    DATE_AND_TIME,
    CPU_TIME