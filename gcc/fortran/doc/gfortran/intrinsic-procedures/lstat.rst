..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _lstat:

LSTAT --- Get file status
*************************

.. index:: LSTAT, file system, file status

.. function:: LSTAT(NAME, VALUES, STATUS)

  ``LSTAT`` is identical to :ref:`STAT`, except that if path is a
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
    See :ref:`STAT` for an example.

  :samp:`{See also}:`
    To stat an open file:
    :ref:`FSTAT`
    To stat a file:
    :ref:`STAT`