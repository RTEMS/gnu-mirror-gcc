..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _kill:

KILL --- Send a signal to a process
***********************************

.. index:: KILL

.. function:: KILL

  Sends the signal specified by :samp:`{SIG}` to the process :samp:`{PID}`.
  See ``kill(2)``.

  :param PID:
    Shall be a scalar ``INTEGER`` with ``INTENT(IN)``.

  :param SIG:
    Shall be a scalar ``INTEGER`` with ``INTENT(IN)``.

  :param STATUS:
    [Subroutine](Optional)
    Shall be a scalar ``INTEGER``.
    Returns 0 on success; otherwise a system-specific error code is returned.

  :param STATUS:
    [Function] The kind type parameter is that of
    ``pid``.
    Returns 0 on success; otherwise a system-specific error code is returned.

  :samp:`{Standard}:`
    GNU extension

  :samp:`{Standard}:`
    GNU extension

  :samp:`{Class}:`
    Subroutine, function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    CALL KILL(PID, SIG [, STATUS])
    STATUS = KILL(PID, SIG)

  :samp:`{See also}:`
    ABORT,
    EXIT