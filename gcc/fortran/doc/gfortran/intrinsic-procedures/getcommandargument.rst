..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _get_command_argument:

GET_COMMAND_ARGUMENT --- Get command line arguments
***************************************************

.. index:: GET_COMMAND_ARGUMENT

.. index:: command-line arguments

.. index:: arguments, to program

.. function:: GET_COMMAND_ARGUMENT

  Retrieve the :samp:`{NUMBER}` -th argument that was passed on the
  command line when the containing program was invoked.

  :param NUMBER:
    Shall be a scalar of type ``INTEGER`` and of
    default kind, :samp:`{NUMBER}` \geq 0

  :param VALUE:
    (Optional) Shall be a scalar of type ``CHARACTER``
    and of default kind.

  :param LENGTH:
    (Optional) Shall be a scalar of type ``INTEGER``
    and of default kind.

  :param STATUS:
    (Optional) Shall be a scalar of type ``INTEGER``
    and of default kind.

  :return:
    After ``GET_COMMAND_ARGUMENT`` returns, the :samp:`{VALUE}` argument holds the
    :samp:`{NUMBER}` -th command line argument. If :samp:`{VALUE}` cannot hold the argument, it is
    truncated to fit the length of :samp:`{VALUE}`. If there are less than :samp:`{NUMBER}`
    arguments specified at the command line, :samp:`{VALUE}` will be filled with blanks.
    If :samp:`{NUMBER}` = 0, :samp:`{VALUE}` is set to the name of the program (on
    systems that support this feature). The :samp:`{LENGTH}` argument contains the
    length of the :samp:`{NUMBER}` -th command line argument. If the argument retrieval
    fails, :samp:`{STATUS}` is a positive number; if :samp:`{VALUE}` contains a truncated
    command line argument, :samp:`{STATUS}` is -1; and otherwise the :samp:`{STATUS}` is
    zero.

  :samp:`{Standard}:`
    Fortran 2003 and later

  :samp:`{Class}:`
    Subroutine

  :samp:`{Syntax}:`

  .. code-block:: fortran

    CALL GET_COMMAND_ARGUMENT(NUMBER [, VALUE, LENGTH, STATUS])

  :samp:`{Example}:`

    .. code-block:: fortran

      PROGRAM test_get_command_argument
        INTEGER :: i
        CHARACTER(len=32) :: arg

        i = 0
        DO
          CALL get_command_argument(i, arg)
          IF (LEN_TRIM(arg) == 0) EXIT

          WRITE (*,*) TRIM(arg)
          i = i+1
        END DO
      END PROGRAM

  :samp:`{See also}:`
    GET_COMMAND,
    COMMAND_ARGUMENT_COUNT