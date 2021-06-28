..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _xor:

XOR --- Bitwise logical exclusive OR
************************************

.. index:: XOR

.. index:: bitwise logical exclusive or

.. index:: logical exclusive or, bitwise

.. function:: XOR

  Bitwise logical exclusive or.

  :param I:
    The type shall be either a scalar ``INTEGER``
    type or a scalar ``LOGICAL`` type or a boz-literal-constant.

  :param J:
    The type shall be the same as the type of :samp:`{I}` or
    a boz-literal-constant. :samp:`{I}` and :samp:`{J}` shall not both be
    boz-literal-constants.  If either :samp:`{I}` and :samp:`{J}` is a
    boz-literal-constant, then the other argument must be a scalar ``INTEGER``.

  :return:
    The return type is either a scalar ``INTEGER`` or a scalar
    ``LOGICAL``.  If the kind type parameters differ, then the
    smaller kind type is implicitly converted to larger kind, and the
    return has the larger kind.  A boz-literal-constant is
    converted to an ``INTEGER`` with the kind type parameter of
    the other argument as-if a call to INT occurred.

  :samp:`{Standard}:`
    GNU extension

  :samp:`{Class}:`
    Function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    RESULT = XOR(I, J)

  :samp:`{Example}:`

    .. code-block:: fortran

      PROGRAM test_xor
        LOGICAL :: T = .TRUE., F = .FALSE.
        INTEGER :: a, b
        DATA a / Z'F' /, b / Z'3' /

        WRITE (*,*) XOR(T, T), XOR(T, F), XOR(F, T), XOR(F, F)
        WRITE (*,*) XOR(a, b)
      END PROGRAM

  :samp:`{See also}:`
    Fortran 95 elemental function:
    IEOR