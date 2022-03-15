..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. index:: LTIME, time, conversion to local time info

.. _ltime:

LTIME --- Convert time to local time info
*****************************************

.. function:: LTIME(TIME, VALUES)

  Given a system time value :samp:`{TIME}` (as provided by the :ref:`TIME`
  intrinsic), fills :samp:`{VALUES}` with values extracted from it appropriate
  to the local time zone using ``localtime(3)``.

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

      CALL LTIME(TIME, VALUES)

  :samp:`{See also}:`
    :ref:`DATE_AND_TIME`,
    :ref:`CTIME`,
    :ref:`GMTIME`,
    :ref:`TIME`,
    :ref:`TIME8`