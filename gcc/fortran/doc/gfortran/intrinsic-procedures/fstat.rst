..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _fstat:

FSTAT --- Get file status
*************************

.. index:: FSTAT

.. index:: file system, file status

.. function:: FSTAT

  ``FSTAT`` is identical to STAT, except that information about an
  already opened file is obtained.

  :param UNIT:
    An open I/O unit number of type ``INTEGER``.

  :param VALUES:
    The type shall be ``INTEGER(4), DIMENSION(13)``.

  :param STATUS:
    (Optional) status flag of type ``INTEGER(4)``. Returns 0
    on success and a system specific error code otherwise.

  :samp:`{Standard}:`
    GNU extension

  :samp:`{Class}:`
    Subroutine, function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    CALL FSTAT(UNIT, VALUES [, STATUS])
    STATUS = FSTAT(UNIT, VALUES)

  :samp:`{Example}:`
    See STAT for an example.

  :samp:`{See also}:`
    To stat a link:
    LSTAT
    To stat a file:
    STAT