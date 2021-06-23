..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _system:

SYSTEM --- Execute a shell command
**********************************

.. index:: SYSTEM

.. index:: system, system call

.. function:: SYSTEM

  Passes the command :samp:`{COMMAND}` to a shell (see ``system(3)`` ). If
  argument :samp:`{STATUS}` is present, it contains the value returned by
  ``system(3)``, which is presumably 0 if the shell command succeeded.
  Note that which shell is used to invoke the command is system-dependent
  and environment-dependent.

  :param COMMAND:
    Shall be of default ``CHARACTER`` type.

  :param STATUS:
    (Optional) Shall be of default ``INTEGER`` type.

  :samp:`{Standard}:`
    GNU extension

  :samp:`{Class}:`
    Subroutine, function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    CALL SYSTEM(COMMAND [, STATUS])
    STATUS = SYSTEM(COMMAND)

  :samp:`{See also}:`
    EXECUTE_COMMAND_LINE, which is part of the Fortran 2008 standard
    and should considered in new code for future portability.