..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _getpid:

GETPID --- Process ID function
******************************

.. index:: GETPID

.. index:: system, process ID

.. index:: process ID

.. function:: GETPID

  Returns the numerical process identifier of the current process.

  :return:
    The return value of ``GETPID`` is an ``INTEGER`` of the default
    kind.

  :samp:`{Standard}:`
    GNU extension

  :samp:`{Class}:`
    Function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    RESULT = GETPID()

  :samp:`{Example}:`

    .. code-block:: fortran

      program info
        print *, "The current process ID is ", getpid()
        print *, "Your numerical user ID is ", getuid()
        print *, "Your numerical group ID is ", getgid()
      end program info

  :samp:`{See also}:`
    GETGID,
    GETUID