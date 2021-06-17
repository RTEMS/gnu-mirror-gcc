..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _link:

LINK --- Create a hard link
***************************

.. index:: LINK

.. index:: file system, create link

.. index:: file system, hard link

.. function:: LINK

  Makes a (hard) link from file :samp:`{PATH1}` to :samp:`{PATH2}`. A null
  character ( ``CHAR(0)`` ) can be used to mark the end of the names in
  :samp:`{PATH1}` and :samp:`{PATH2}` ; otherwise, trailing blanks in the file
  names are ignored.  If the :samp:`{STATUS}` argument is supplied, it
  contains 0 on success or a nonzero error code upon return; see
  ``link(2)``.

  :param PATH1:
    Shall be of default ``CHARACTER`` type.

  :param PATH2:
    Shall be of default ``CHARACTER`` type.

  :param STATUS:
    (Optional) Shall be of default ``INTEGER`` type.

  :samp:`{Standard}:`
    GNU extension

  :samp:`{Class}:`
    Subroutine, function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    CALL LINK(PATH1, PATH2 [, STATUS])
    STATUS = LINK(PATH1, PATH2)

  :samp:`{See also}:`
    SYMLNK,
    UNLINK