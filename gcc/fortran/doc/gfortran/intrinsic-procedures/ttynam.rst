..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _ttynam:

TTYNAM --- Get the name of a terminal device.
*********************************************

.. index:: TTYNAM

.. index:: system, terminal

.. function:: TTYNAM

  Get the name of a terminal device. For more information,
  see ``ttyname(3)``.

  :param UNIT:
    Shall be a scalar ``INTEGER``.

  :param NAME:
    Shall be of type ``CHARACTER``.

  :samp:`{Standard}:`
    GNU extension

  :samp:`{Class}:`
    Subroutine, function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    CALL TTYNAM(UNIT, NAME)
    NAME = TTYNAM(UNIT)

  :samp:`{Example}:`

    .. code-block:: fortran

      PROGRAM test_ttynam
        INTEGER :: unit
        DO unit = 1, 10
          IF (isatty(unit=unit)) write(*,*) ttynam(unit)
        END DO
      END PROGRAM

  :samp:`{See also}:`
    ISATTY