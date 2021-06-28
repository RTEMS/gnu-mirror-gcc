..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _perror:

PERROR --- Print system error message
*************************************

.. index:: PERROR

.. index:: system, error handling

.. function:: PERROR

  Prints (on the C ``stderr`` stream) a newline-terminated error
  message corresponding to the last system error. This is prefixed by
  :samp:`{STRING}`, a colon and a space. See ``perror(3)``.

  :param STRING:
    A scalar of type ``CHARACTER`` and of the
    default kind.

  :samp:`{Standard}:`
    GNU extension

  :samp:`{Class}:`
    Subroutine

  :samp:`{Syntax}:`

  .. code-block:: fortran

    CALL PERROR(STRING)

  :samp:`{See also}:`
    IERRNO