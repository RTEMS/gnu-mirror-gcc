..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _lnblnk:

LNBLNK --- Index of the last non-blank character in a string
************************************************************

.. index:: LNBLNK

.. index:: string, find non-blank character

.. function:: LNBLNK

  Returns the length of a character string, ignoring any trailing blanks.
  This is identical to the standard ``LEN_TRIM`` intrinsic, and is only
  included for backwards compatibility.

  :param STRING:
    Shall be a scalar of type ``CHARACTER``,
    with ``INTENT(IN)``

  :return:
    The return value is of ``INTEGER(kind=4)`` type.

  :samp:`{Standard}:`
    GNU extension

  :samp:`{Class}:`
    Elemental function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    RESULT = LNBLNK(STRING)

  :samp:`{See also}:`
    INDEX intrinsic,
    LEN_TRIM