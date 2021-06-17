..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _fgetc:

FGETC --- Read a single character in stream mode
************************************************

.. index:: FGETC

.. index:: read character, stream mode

.. index:: stream mode, read character

.. index:: file operation, read character

.. function:: FGETC

  Read a single character in stream mode by bypassing normal formatted output.
  Stream I/O should not be mixed with normal record-oriented (formatted or
  unformatted) I/O on the same unit; the results are unpredictable.

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

    CALL FGETC(UNIT, C [, STATUS])
    STATUS = FGETC(UNIT, C)

  :samp:`{Example}:`

    .. code-block:: fortran

      PROGRAM test_fgetc
        INTEGER :: fd = 42, status
        CHARACTER :: c

        OPEN(UNIT=fd, FILE="/etc/passwd", ACTION="READ", STATUS = "OLD")
        DO
          CALL fgetc(fd, c, status)
          IF (status /= 0) EXIT
          call fput(c)
        END DO
        CLOSE(UNIT=fd)
      END PROGRAM

  :samp:`{See also}:`
    FGET,
    FPUT,
    FPUTC