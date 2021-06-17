..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _present:

PRESENT --- Determine whether an optional dummy argument is specified
*********************************************************************

.. index:: PRESENT

.. function:: PRESENT

  Determines whether an optional dummy argument is present.

  :param A:
    May be of any type and may be a pointer, scalar or array
    value, or a dummy procedure. It shall be the name of an optional dummy argument
    accessible within the current subroutine or function.

  :return:
    Returns either ``TRUE`` if the optional argument :samp:`{A}` is present, or
    ``FALSE`` otherwise.

  :samp:`{Standard}:`
    Fortran 90 and later

  :samp:`{Class}:`
    Inquiry function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    RESULT = PRESENT(A)

  :samp:`{Example}:`

    .. code-block:: fortran

      PROGRAM test_present
        WRITE(*,*) f(), f(42)      ! "F T"
      CONTAINS
        LOGICAL FUNCTION f(x)
          INTEGER, INTENT(IN), OPTIONAL :: x
          f = PRESENT(x)
        END FUNCTION
      END PROGRAM