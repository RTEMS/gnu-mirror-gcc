..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _fputc:

FPUTC --- Write a single character in stream mode
*************************************************

.. index:: FPUTC

.. index:: write character, stream mode

.. index:: stream mode, write character

.. index:: file operation, write character

.. function:: FPUTC

  Write a single character in stream mode by bypassing normal formatted
  output. Stream I/O should not be mixed with normal record-oriented
  (formatted or unformatted) I/O on the same unit; the results are unpredictable.

  :param UNIT:
    The type shall be ``INTEGER``.

  :param C:
    The type shall be ``CHARACTER`` and of default
    kind.

  :param STATUS:
    (Optional) status flag of type ``INTEGER``.
    Returns 0 on success, -1 on end-of-file and a system specific positive
    error code otherwise.

  :samp:`{Standard}:`
    GNU extension

  :samp:`{Class}:`
    Subroutine, function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    CALL FPUTC(UNIT, C [, STATUS])
    STATUS = FPUTC(UNIT, C)

  :samp:`{Example}:`

    .. code-block:: fortran

      PROGRAM test_fputc
        CHARACTER(len=10) :: str = "gfortran"
        INTEGER :: fd = 42, i

        OPEN(UNIT = fd, FILE = "out", ACTION = "WRITE", STATUS="NEW")
        DO i = 1, len_trim(str)
          CALL fputc(fd, str(i:i))
        END DO
        CLOSE(fd)
      END PROGRAM

  :samp:`{See also}:`
    FPUT,
    FGET,
    FGETC