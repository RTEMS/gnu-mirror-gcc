..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _c_funloc:

C_FUNLOC --- Obtain the C address of a procedure
************************************************

.. index:: C_FUNLOC

.. index:: pointer, C address of procedures

.. function:: C_FUNLOC(x)

  ``C_FUNLOC(x)`` determines the C address of the argument.

  :param x:
    Interoperable function or pointer to such function.

  :return:
    The return value is of type ``C_FUNPTR`` and contains the C address
    of the argument.

  :samp:`{Standard}:`
    Fortran 2003 and later

  :samp:`{Class}:`
    Inquiry function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    RESULT = C_FUNLOC(x)

  :samp:`{Example}:`

    .. code-block:: fortran

      module x
        use iso_c_binding
        implicit none
      contains
        subroutine sub(a) bind(c)
          real(c_float) :: a
          a = sqrt(a)+5.0
        end subroutine sub
      end module x
      program main
        use iso_c_binding
        use x
        implicit none
        interface
          subroutine my_routine(p) bind(c,name='myC_func')
            import :: c_funptr
            type(c_funptr), intent(in) :: p
          end subroutine
        end interface
        call my_routine(c_funloc(sub))
      end program main

  :samp:`{See also}:`
    C_ASSOCIATED,
    C_LOC,
    C_F_POINTER,
    C_F_PROCPOINTER