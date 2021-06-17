..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _max:

MAX --- Maximum value of an argument list
*****************************************

.. index:: MAX

.. index:: MAX0

.. index:: AMAX0

.. index:: MAX1

.. index:: AMAX1

.. index:: DMAX1

.. index:: maximum value

.. function:: MAX

  Returns the argument with the largest (most positive) value.

  :param A1:
    The type shall be ``INTEGER`` or
    ``REAL``.

  :param A2}, {A3}, ...:
    An expression of the same type and kind
    as :samp:`{A1}`.  (As a GNU extension, arguments of different kinds are
    permitted.)

  :return:
    The return value corresponds to the maximum value among the arguments,
    and has the same type and kind as the first argument.

  :samp:`{Standard}:`
    Fortran 77 and later

  :samp:`{Class}:`
    Elemental function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    RESULT = MAX(A1, A2 [, A3 [, ...]])

  :samp:`{Specific names}:`
    =============  =================  ================  ====================
    Name           Argument           Return type       Standard
    =============  =================  ================  ====================
    ``MAX0(A1)``   ``INTEGER(4) A1``  ``INTEGER(4)``    Fortran 77 and later
    ``AMAX0(A1)``  ``INTEGER(4) A1``  ``REAL(MAX(X))``  Fortran 77 and later
    ``MAX1(A1)``   ``REAL A1``        ``INT(MAX(X))``   Fortran 77 and later
    ``AMAX1(A1)``  ``REAL(4) A1``     ``REAL(4)``       Fortran 77 and later
    ``DMAX1(A1)``  ``REAL(8) A1``     ``REAL(8)``       Fortran 77 and later
    =============  =================  ================  ====================

  :samp:`{See also}:`
    MAXLOC
    MAXVAL,
    MIN