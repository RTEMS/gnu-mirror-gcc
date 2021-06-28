..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _ftell:

FTELL --- Current stream position
*********************************

.. index:: FTELL

.. index:: file operation, position

.. function:: FTELL

  Retrieves the current position within an open file.

  :param OFFSET:
    Shall of type ``INTEGER``.

  :param UNIT:
    Shall of type ``INTEGER``.

  :return:
    In either syntax, :samp:`{OFFSET}` is set to the current offset of unit
    number :samp:`{UNIT}`, or to -1 if the unit is not currently open.

  :samp:`{Standard}:`
    GNU extension

  :samp:`{Class}:`
    Subroutine, function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    CALL FTELL(UNIT, OFFSET)
    OFFSET = FTELL(UNIT)

  :samp:`{Example}:`

    .. code-block:: fortran

      PROGRAM test_ftell
        INTEGER :: i
        OPEN(10, FILE="temp.dat")
        CALL ftell(10,i)
        WRITE(*,*) i
      END PROGRAM

  :samp:`{See also}:`
    FSEEK