..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _getenv:

GETENV --- Get an environmental variable
****************************************

.. index:: GETENV

.. index:: environment variable

.. function:: GETENV

  Get the :samp:`{VALUE}` of the environmental variable :samp:`{NAME}`.

  :param NAME:
    Shall be of type ``CHARACTER`` and of default kind.

  :param VALUE:
    Shall be of type ``CHARACTER`` and of default kind.

  :return:
    Stores the value of :samp:`{NAME}` in :samp:`{VALUE}`. If :samp:`{VALUE}` is
    not large enough to hold the data, it is truncated. If :samp:`{NAME}`
    is not set, :samp:`{VALUE}` will be filled with blanks.

  :samp:`{Standard}:`
    GNU extension

  :samp:`{Class}:`
    Subroutine

  :samp:`{Syntax}:`

  .. code-block:: fortran

    CALL GETENV(NAME, VALUE)

  :samp:`{Example}:`

    .. code-block:: fortran

      PROGRAM test_getenv
        CHARACTER(len=255) :: homedir
        CALL getenv("HOME", homedir)
        WRITE (*,*) TRIM(homedir)
      END PROGRAM

  :samp:`{See also}:`
    GET_ENVIRONMENT_VARIABLE