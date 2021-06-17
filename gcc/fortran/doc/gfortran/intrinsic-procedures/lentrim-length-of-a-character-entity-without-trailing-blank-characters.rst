..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _len_trim:

LEN_TRIM --- Length of a character entity without trailing blank characters
***************************************************************************

.. index:: LEN_TRIM

.. index:: string, length, without trailing whitespace

.. function:: LEN_TRIM

  Returns the length of a character string, ignoring any trailing blanks.

  :param STRING:
    Shall be a scalar of type ``CHARACTER``,
    with ``INTENT(IN)``

  :param KIND:
    (Optional) An ``INTEGER`` initialization
    expression indicating the kind parameter of the result.

  :return:
    The return value is of type ``INTEGER`` and of kind :samp:`{KIND}`. If
    :samp:`{KIND}` is absent, the return value is of default integer kind.

  :samp:`{Standard}:`
    Fortran 90 and later, with :samp:`{KIND}` argument Fortran 2003 and later

  :samp:`{Class}:`
    Elemental function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    RESULT = LEN_TRIM(STRING [, KIND])

  :samp:`{See also}:`
    LEN,
    ADJUSTL,
    ADJUSTR