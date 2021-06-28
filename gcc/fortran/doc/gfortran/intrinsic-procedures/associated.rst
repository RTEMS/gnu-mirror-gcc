..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _associated:

ASSOCIATED --- Status of a pointer or pointer/target pair
**********************************************************

.. index:: ASSOCIATED

.. index:: pointer, status

.. index:: association status

.. function:: ASSOCIATED(POINTER [, TARGET])

  ``ASSOCIATED(POINTER [, TARGET])`` determines the status of the pointer
  :samp:`{POINTER}` or if :samp:`{POINTER}` is associated with the target :samp:`{TARGET}`.

  :param POINTER:
    :samp:`{POINTER}` shall have the ``POINTER`` attribute
    and it can be of any type.

  :param TARGET:
    (Optional) :samp:`{TARGET}` shall be a pointer or
    a target.  It must have the same type, kind type parameter, and
    array rank as :samp:`{POINTER}`.

  :return:
    ``ASSOCIATED(POINTER)`` returns a scalar value of type ``LOGICAL(4)``.
    There are several cases:

  :samp:`{Standard}:`
    Fortran 90 and later

  :samp:`{Class}:`
    Inquiry function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    RESULT = ASSOCIATED(POINTER [, TARGET])

  :samp:`{Example}:`

    .. code-block:: fortran

      program test_associated
         implicit none
         real, target  :: tgt(2) = (/1., 2./)
         real, pointer :: ptr(:)
         ptr => tgt
         if (associated(ptr)     .eqv. .false.) call abort
         if (associated(ptr,tgt) .eqv. .false.) call abort
      end program test_associated

  :samp:`{See also}:`
    NULL