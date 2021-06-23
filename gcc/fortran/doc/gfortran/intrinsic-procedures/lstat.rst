..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _lstat:

LSTAT --- Get file status
*************************

.. index:: LSTAT

.. index:: file system, file status

.. function:: LSTAT

  ``LSTAT`` is identical to STAT, except that if path is a
  symbolic link, then the link itself is statted, not the file that it
  refers to.

  :param NAME:
    The type shall be ``CHARACTER`` of the default
    kind, a valid path within the file system.

  :param VALUES:
    The type shall be ``INTEGER(4), DIMENSION(13)``.

  :param STATUS:
    (Optional) status flag of type ``INTEGER(4)``.
    Returns 0 on success and a system specific error code otherwise.

  :samp:`{Standard}:`
    GNU extension

  :samp:`{Class}:`
    Subroutine, function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    CALL LSTAT(NAME, VALUES [, STATUS])
    STATUS = LSTAT(NAME, VALUES)

  :samp:`{Example}:`
    See STAT for an example.

  :samp:`{See also}:`
    To stat an open file:
    FSTAT
    To stat a file:
    STAT