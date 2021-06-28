..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _ierrno:

IERRNO --- Get the last system error number
*******************************************

.. index:: IERRNO

.. index:: system, error handling

.. function:: IERRNO

  Returns the last system error number, as given by the C ``errno``
  variable.

  :return:
    The return value is of type ``INTEGER`` and of the default integer
    kind.

  :samp:`{Standard}:`
    GNU extension

  :samp:`{Class}:`
    Function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    RESULT = IERRNO()

  :samp:`{Arguments}:`
    None

  :samp:`{See also}:`
    PERROR