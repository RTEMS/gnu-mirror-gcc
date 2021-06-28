..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _gmtime:

GMTIME --- Convert time to GMT info
***********************************

.. index:: GMTIME

.. index:: time, conversion to GMT info

.. function:: GMTIME

  Given a system time value :samp:`{TIME}` (as provided by the TIME
  intrinsic), fills :samp:`{VALUES}` with values extracted from it appropriate
  to the UTC time zone (Universal Coordinated Time, also known in some
  countries as GMT, Greenwich Mean Time), using ``gmtime(3)``.

  :param TIME:
    An ``INTEGER`` scalar expression
    corresponding to a system time, with ``INTENT(IN)``.

  :param VALUES:
    A default ``INTEGER`` array with 9 elements,
    with ``INTENT(OUT)``.

  :return:
    The elements of :samp:`{VALUES}` are assigned as follows:

  :samp:`{Standard}:`
    GNU extension

  :samp:`{Class}:`
    Subroutine

  :samp:`{Syntax}:`

  .. code-block:: fortran

    CALL GMTIME(TIME, VALUES)

  :samp:`{See also}:`
    DATE_AND_TIME,
    CTIME,
    LTIME,
    TIME,
    TIME8