..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. index:: MIN, MIN0, AMIN0, MIN1, AMIN1, DMIN1, minimum value

.. _min:

MIN --- Minimum value of an argument list
*****************************************

.. function:: MIN(A1, A2 , A3, ...)

  Returns the argument with the smallest (most negative) value.

  :param A1:
    The type shall be ``INTEGER`` or
    ``REAL``.

  :param A2}, {A3}, ...:
    An expression of the same type and kind
    as :samp:`{A1}`.  (As a GNU extension, arguments of different kinds are
    permitted.)

  :return:
    The return value corresponds to the minimum value among the arguments,
    and has the same type and kind as the first argument.

  :samp:`{Standard}:`
    Fortran 77 and later

  :samp:`{Class}:`
    Elemental function

  :samp:`{Syntax}:`

    .. code-block:: fortran

      RESULT = MIN(A1, A2 [, A3, ...])

  :samp:`{Specific names}:`
    =============  =================  ==============  ====================
    Name           Argument           Return type     Standard
    =============  =================  ==============  ====================
    ``MIN0(A1)``   ``INTEGER(4) A1``  ``INTEGER(4)``  Fortran 77 and later
    ``AMIN0(A1)``  ``INTEGER(4) A1``  ``REAL(4)``     Fortran 77 and later
    ``MIN1(A1)``   ``REAL A1``        ``INTEGER(4)``  Fortran 77 and later
    ``AMIN1(A1)``  ``REAL(4) A1``     ``REAL(4)``     Fortran 77 and later
    ``DMIN1(A1)``  ``REAL(8) A1``     ``REAL(8)``     Fortran 77 and later
    =============  =================  ==============  ====================

  :samp:`{See also}:`
    :ref:`MAX`,
    :ref:`MINLOC`,
    :ref:`MINVAL`