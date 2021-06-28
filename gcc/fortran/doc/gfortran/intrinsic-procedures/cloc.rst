..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _c_loc:

C_LOC --- Obtain the C address of an object
*******************************************

.. index:: C_LOC

.. index:: procedure pointer, convert C to Fortran

.. function:: C_LOC(X)

  ``C_LOC(X)`` determines the C address of the argument.

  :param X:
    Shall have either the POINTER or TARGET attribute. It shall not be a coindexed object. It shall either be a variable with interoperable type and kind type parameters, or be a scalar, nonpolymorphic variable with no length type parameters.

  :return:
    The return value is of type ``C_PTR`` and contains the C address
    of the argument.

  :samp:`{Standard}:`
    Fortran 2003 and later

  :samp:`{Class}:`
    Inquiry function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    RESULT = C_LOC(X)

  :samp:`{Example}:`

    .. code-block:: fortran

      subroutine association_test(a,b)
        use iso_c_binding, only: c_associated, c_loc, c_ptr
        implicit none
        real, pointer :: a
        type(c_ptr) :: b
        if(c_associated(b, c_loc(a))) &
           stop 'b and a do not point to same target'
      end subroutine association_test

  :samp:`{See also}:`
    C_ASSOCIATED,
    C_FUNLOC,
    C_F_POINTER,
    C_F_PROCPOINTER