..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _free:

FREE --- Frees memory
*********************

.. index:: FREE

.. index:: pointer, cray

.. function:: FREE

  Frees memory previously allocated by ``MALLOC``. The ``FREE``
  intrinsic is an extension intended to be used with Cray pointers, and is
  provided in GNU Fortran to allow user to compile legacy code. For
  new code using Fortran 95 pointers, the memory de-allocation intrinsic is
  ``DEALLOCATE``.

  :param PTR:
    The type shall be ``INTEGER``. It represents the
    location of the memory that should be de-allocated.

  :return:
    None

  :samp:`{Standard}:`
    GNU extension

  :samp:`{Class}:`
    Subroutine

  :samp:`{Syntax}:`

  .. code-block:: fortran

    CALL FREE(PTR)

  :samp:`{Example}:`
    See ``MALLOC`` for an example.

  :samp:`{See also}:`
    MALLOC