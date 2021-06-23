..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _fput:

FPUT --- Write a single character in stream mode to stdout
***********************************************************

.. index:: FPUT

.. index:: write character, stream mode

.. index:: stream mode, write character

.. index:: file operation, write character

.. function:: FPUT

  Write a single character in stream mode to stdout by bypassing normal
  formatted output. Stream I/O should not be mixed with normal record-oriented
  (formatted or unformatted) I/O on the same unit; the results are unpredictable.

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

    CALL FPUT(C [, STATUS])
    STATUS = FPUT(C)

  :samp:`{Example}:`

    .. code-block:: fortran

      PROGRAM test_fput
        CHARACTER(len=10) :: str = "gfortran"
        INTEGER :: i
        DO i = 1, len_trim(str)
          CALL fput(str(i:i))
        END DO
      END PROGRAM

  :samp:`{See also}:`
    FPUTC,
    FGET,
    FGETC