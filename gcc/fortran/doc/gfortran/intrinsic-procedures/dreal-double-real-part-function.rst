..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _dreal:

DREAL --- Double real part function
***********************************

.. index:: DREAL

.. index:: complex numbers, real part

.. function:: DREAL(Z)

  ``DREAL(Z)`` returns the real part of complex variable :samp:`{Z}`.

  :param A:
    The type shall be ``COMPLEX(8)``.

  :return:
    The return value is of type ``REAL(8)``.

  :samp:`{Standard}:`
    GNU extension

  :samp:`{Class}:`
    Elemental function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    RESULT = DREAL(A)

  :samp:`{Example}:`

    .. code-block:: fortran

      program test_dreal
          complex(8) :: z = (1.3_8,7.2_8)
          print *, dreal(z)
      end program test_dreal

  :samp:`{See also}:`
    AIMAG