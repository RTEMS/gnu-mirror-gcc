..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _getlog:

GETLOG --- Get login name
*************************

.. index:: GETLOG

.. index:: system, login name

.. index:: login name

.. function:: GETLOG

  Gets the username under which the program is running.

  :param C:
    Shall be of type ``CHARACTER`` and of default kind.

  :return:
    Stores the current user name in :samp:`{C}`.  (On systems where POSIX
    functions ``geteuid`` and ``getpwuid`` are not available, and
    the ``getlogin`` function is not implemented either, this will
    return a blank string.)

  :samp:`{Standard}:`
    GNU extension

  :samp:`{Class}:`
    Subroutine

  :samp:`{Syntax}:`

  .. code-block:: fortran

    CALL GETLOG(C)

  :samp:`{Example}:`

    .. code-block:: fortran

      PROGRAM TEST_GETLOG
        CHARACTER(32) :: login
        CALL GETLOG(login)
        WRITE(*,*) login
      END PROGRAM

  :samp:`{See also}:`
    GETUID