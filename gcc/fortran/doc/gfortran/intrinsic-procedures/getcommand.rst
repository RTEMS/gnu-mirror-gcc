..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _get_command:

GET_COMMAND --- Get the entire command line
*******************************************

.. index:: GET_COMMAND

.. index:: command-line arguments

.. index:: arguments, to program

.. function:: GET_COMMAND

  Retrieve the entire command line that was used to invoke the program.

  :param COMMAND:
    (Optional) shall be of type ``CHARACTER`` and
    of default kind.

  :param LENGTH:
    (Optional) Shall be of type ``INTEGER`` and of
    default kind.

  :param STATUS:
    (Optional) Shall be of type ``INTEGER`` and of
    default kind.

  :return:
    If :samp:`{COMMAND}` is present, stores the entire command line that was used
    to invoke the program in :samp:`{COMMAND}`. If :samp:`{LENGTH}` is present, it is
    assigned the length of the command line. If :samp:`{STATUS}` is present, it
    is assigned 0 upon success of the command, -1 if :samp:`{COMMAND}` is too
    short to store the command line, or a positive value in case of an error.

  :samp:`{Standard}:`
    Fortran 2003 and later

  :samp:`{Class}:`
    Subroutine

  :samp:`{Syntax}:`

  .. code-block:: fortran

    CALL GET_COMMAND([COMMAND, LENGTH, STATUS])

  :samp:`{Example}:`

    .. code-block:: fortran

      PROGRAM test_get_command
        CHARACTER(len=255) :: cmd
        CALL get_command(cmd)
        WRITE (*,*) TRIM(cmd)
      END PROGRAM

  :samp:`{See also}:`
    GET_COMMAND_ARGUMENT,
    COMMAND_ARGUMENT_COUNT