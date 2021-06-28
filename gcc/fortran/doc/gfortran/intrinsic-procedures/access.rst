..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _access:

ACCESS --- Checks file access modes
***********************************

.. index:: ACCESS

.. index:: file system, access mode

.. function:: ACCESS(NAME, MODE)

  ``ACCESS(NAME, MODE)`` checks whether the file :samp:`{NAME}`
  exists, is readable, writable or executable. Except for the
  executable check, ``ACCESS`` can be replaced by
  Fortran 95's ``INQUIRE``.

  :param NAME:
    Scalar ``CHARACTER`` of default kind with the
    file name. Trailing blank are ignored unless the character ``achar(0)``
    is present, then all characters up to and excluding ``achar(0)`` are
    used as file name.

  :param MODE:
    Scalar ``CHARACTER`` of default kind with the
    file access mode, may be any concatenation of ``"r"`` (readable),
    ``"w"`` (writable) and ``"x"`` (executable), or ``" "`` to check
    for existence.

  :return:
    Returns a scalar ``INTEGER``, which is ``0`` if the file is
    accessible in the given mode; otherwise or if an invalid argument
    has been given for ``MODE`` the value ``1`` is returned.

  :samp:`{Standard}:`
    GNU extension

  :samp:`{Class}:`
    Inquiry function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    RESULT = ACCESS(NAME, MODE)

  :samp:`{Example}:`

    .. code-block:: fortran

      program access_test
        implicit none
        character(len=*), parameter :: file  = 'test.dat'
        character(len=*), parameter :: file2 = 'test.dat  '//achar(0)
        if(access(file,' ') == 0) print *, trim(file),' is exists'
        if(access(file,'r') == 0) print *, trim(file),' is readable'
        if(access(file,'w') == 0) print *, trim(file),' is writable'
        if(access(file,'x') == 0) print *, trim(file),' is executable'
        if(access(file2,'rwx') == 0) &
          print *, trim(file2),' is readable, writable and executable'
      end program access_test