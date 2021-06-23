..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _abort:

ABORT --- Abort the program
***************************

.. index:: ABORT

.. index:: program termination, with core dump

.. index:: terminate program, with core dump

.. index:: core, dump

.. function:: ABORT

  ``ABORT`` causes immediate termination of the program.  On operating
  systems that support a core dump, ``ABORT`` will produce a core dump.
  It will also print a backtrace, unless ``-fno-backtrace`` is given.

  :return:
    Does not return.

  :samp:`{Standard}:`
    GNU extension

  :samp:`{Class}:`
    Subroutine

  :samp:`{Syntax}:`

  .. code-block:: fortran

    CALL ABORT

  :samp:`{Example}:`

    .. code-block:: fortran

      program test_abort
        integer :: i = 1, j = 2
        if (i /= j) call abort
      end program test_abort

  :samp:`{See also}:`
    EXIT,
    KILL,
    BACKTRACE