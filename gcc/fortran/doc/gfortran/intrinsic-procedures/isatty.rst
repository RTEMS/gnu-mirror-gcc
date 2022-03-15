..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. index:: ISATTY, system, terminal

.. _isatty:

ISATTY --- Whether a unit is a terminal device.
***********************************************

.. function:: ISATTY(UNIT)

  Determine whether a unit is connected to a terminal device.

  :param UNIT:
    Shall be a scalar ``INTEGER``.

  :return:
    Returns ``.TRUE.`` if the :samp:`{UNIT}` is connected to a terminal
    device, ``.FALSE.`` otherwise.

  :samp:`{Standard}:`
    GNU extension

  :samp:`{Class}:`
    Function

  :samp:`{Syntax}:`

    .. code-block:: fortran

      RESULT = ISATTY(UNIT)

  :samp:`{Example}:`

    .. code-block:: fortran

      PROGRAM test_isatty
        INTEGER(kind=1) :: unit
        DO unit = 1, 10
          write(*,*) isatty(unit=unit)
        END DO
      END PROGRAM

  :samp:`{See also}:`
    :ref:`TTYNAM`