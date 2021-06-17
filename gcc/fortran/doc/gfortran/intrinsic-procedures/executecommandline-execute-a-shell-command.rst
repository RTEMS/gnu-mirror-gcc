..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _execute_command_line:

EXECUTE_COMMAND_LINE --- Execute a shell command
************************************************

.. index:: EXECUTE_COMMAND_LINE

.. index:: system, system call

.. index:: command line

.. function:: EXECUTE_COMMAND_LINE

  ``EXECUTE_COMMAND_LINE`` runs a shell command, synchronously or
  asynchronously.

  :param COMMAND:
    Shall be a default ``CHARACTER`` scalar.

  :param WAIT:
    (Optional) Shall be a default ``LOGICAL`` scalar.

  :param EXITSTAT:
    (Optional) Shall be an ``INTEGER`` of the
    default kind.

  :param CMDSTAT:
    (Optional) Shall be an ``INTEGER`` of the
    default kind.

  :param CMDMSG:
    (Optional) Shall be an ``CHARACTER`` scalar of the
    default kind.

  :samp:`{Standard}:`
    Fortran 2008 and later

  :samp:`{Class}:`
    Subroutine

  :samp:`{Syntax}:`

  .. code-block:: fortran

    CALL EXECUTE_COMMAND_LINE(COMMAND [, WAIT, EXITSTAT, CMDSTAT, CMDMSG ])

  :samp:`{Example}:`

    .. code-block:: fortran

      program test_exec
        integer :: i

        call execute_command_line ("external_prog.exe", exitstat=i)
        print *, "Exit status of external_prog.exe was ", i

        call execute_command_line ("reindex_files.exe", wait=.false.)
        print *, "Now reindexing files in the background"

      end program test_exec

  :samp:`{Note}:`
    Because this intrinsic is implemented in terms of the ``system``
    function call, its behavior with respect to signaling is processor
    dependent. In particular, on POSIX-compliant systems, the SIGINT and
    SIGQUIT signals will be ignored, and the SIGCHLD will be blocked. As
    such, if the parent process is terminated, the child process might not be
    terminated alongside.

  :samp:`{See also}:`
    SYSTEM