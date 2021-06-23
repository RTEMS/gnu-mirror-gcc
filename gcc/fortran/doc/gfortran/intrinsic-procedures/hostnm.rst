..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _hostnm:

HOSTNM --- Get system host name
*******************************

.. index:: HOSTNM

.. index:: system, host name

.. function:: HOSTNM

  Retrieves the host name of the system on which the program is running.

  :param C:
    Shall of type ``CHARACTER`` and of default kind.

  :param STATUS:
    (Optional) status flag of type ``INTEGER``.
    Returns 0 on success, or a system specific error code otherwise.

  :return:
    In either syntax, :samp:`{NAME}` is set to the current hostname if it can
    be obtained, or to a blank string otherwise.

  :samp:`{Standard}:`
    GNU extension

  :samp:`{Class}:`
    Subroutine, function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    CALL HOSTNM(C [, STATUS])
    STATUS = HOSTNM(NAME)