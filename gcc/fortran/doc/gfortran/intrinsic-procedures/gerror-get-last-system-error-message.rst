..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _gerror:

GERROR --- Get last system error message
****************************************

.. index:: GERROR

.. index:: system, error handling

.. function:: GERROR

  Returns the system error message corresponding to the last system error.
  This resembles the functionality of ``strerror(3)`` in C.

  :param RESULT:
    Shall be of type ``CHARACTER`` and of default kind.

  :samp:`{Standard}:`
    GNU extension

  :samp:`{Class}:`
    Subroutine

  :samp:`{Syntax}:`

  .. code-block:: fortran

    CALL GERROR(RESULT)

  :samp:`{Example}:`

    .. code-block:: fortran

      PROGRAM test_gerror
        CHARACTER(len=100) :: msg
        CALL gerror(msg)
        WRITE(*,*) msg
      END PROGRAM

  :samp:`{See also}:`
    IERRNO,
    PERROR