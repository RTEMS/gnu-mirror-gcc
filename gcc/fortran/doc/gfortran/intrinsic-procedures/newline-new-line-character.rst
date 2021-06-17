..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _new_line:

NEW_LINE --- New line character
*******************************

.. index:: NEW_LINE

.. index:: newline

.. index:: output, newline

.. function:: NEW_LINE(C)

  ``NEW_LINE(C)`` returns the new-line character.

  :param C:
    The argument shall be a scalar or array of the
    type ``CHARACTER``.

  :return:
    Returns a :samp:`{CHARACTER}` scalar of length one with the new-line character of
    the same kind as parameter :samp:`{C}`.

  :samp:`{Standard}:`
    Fortran 2003 and later

  :samp:`{Class}:`
    Inquiry function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    RESULT = NEW_LINE(C)

  :samp:`{Example}:`

    .. code-block:: fortran

      program newline
        implicit none
        write(*,'(A)') 'This is record 1.'//NEW_LINE('A')//'This is record 2.'
      end program newline