..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _chdir:

CHDIR --- Change working directory
**********************************

.. index:: CHDIR

.. index:: system, working directory

.. function:: CHDIR

  Change current working directory to a specified path.

  :param NAME:
    The type shall be ``CHARACTER`` of default
    kind and shall specify a valid path within the file system.

  :param STATUS:
    (Optional) ``INTEGER`` status flag of the default
    kind.  Returns 0 on success, and a system specific and nonzero error code
    otherwise.

  :samp:`{Standard}:`
    GNU extension

  :samp:`{Class}:`
    Subroutine, function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    CALL CHDIR(NAME [, STATUS])
    STATUS = CHDIR(NAME)

  :samp:`{Example}:`

    .. code-block:: fortran

      PROGRAM test_chdir
        CHARACTER(len=255) :: path
        CALL getcwd(path)
        WRITE(*,*) TRIM(path)
        CALL chdir("/tmp")
        CALL getcwd(path)
        WRITE(*,*) TRIM(path)
      END PROGRAM

  :samp:`{See also}:`
    GETCWD