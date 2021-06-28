..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _loc:

LOC --- Returns the address of a variable
*****************************************

.. index:: LOC

.. index:: location of a variable in memory

.. function:: LOC(X)

  ``LOC(X)`` returns the address of :samp:`{X}` as an integer.

  :param X:
    Variable of any type.

  :return:
    The return value is of type ``INTEGER``, with a ``KIND``
    corresponding to the size (in bytes) of a memory address on the target
    machine.

  :samp:`{Standard}:`
    GNU extension

  :samp:`{Class}:`
    Inquiry function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    RESULT = LOC(X)

  :samp:`{Example}:`

    .. code-block:: fortran

      program test_loc
        integer :: i
        real :: r
        i = loc(r)
        print *, i
      end program test_loc