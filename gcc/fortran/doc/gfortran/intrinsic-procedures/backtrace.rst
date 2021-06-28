..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _backtrace:

BACKTRACE --- Show a backtrace
******************************

.. index:: BACKTRACE

.. index:: backtrace

.. function:: BACKTRACE

  ``BACKTRACE`` shows a backtrace at an arbitrary place in user code. Program
  execution continues normally afterwards. The backtrace information is printed
  to the unit corresponding to ``ERROR_UNIT`` in ``ISO_FORTRAN_ENV``.

  :samp:`{Standard}:`
    GNU extension

  :samp:`{Class}:`
    Subroutine

  :samp:`{Syntax}:`

  .. code-block:: fortran

    CALL BACKTRACE

  :samp:`{Arguments}:`
    None

  :samp:`{See also}:`
    ABORT