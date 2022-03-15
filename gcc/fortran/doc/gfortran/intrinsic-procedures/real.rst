..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _real:

REAL --- Convert to real type
******************************

.. index:: REAL, REALPART, FLOAT, DFLOAT, FLOATI, FLOATJ, FLOATK, SNGL, conversion, to real, complex numbers, real part

.. function:: REAL(A, KIND)

  ``REAL(A [, KIND])`` converts its argument :samp:`{A}` to a real type.  The
  ``REALPART`` function is provided for compatibility with :command:`g77`,
  and its use is strongly discouraged.

  :param A:
    Shall be ``INTEGER``, ``REAL``, or
    ``COMPLEX``.

  :param KIND:
    (Optional) An ``INTEGER`` initialization
    expression indicating the kind parameter of the result.

  :return:
    These functions return a ``REAL`` variable or array under
    the following rules:

  :samp:`{Standard}:`
    Fortran 77 and later, with :samp:`{KIND}` argument Fortran 90 and later, has GNU extensions

  :samp:`{Class}:`
    Elemental function

  :samp:`{Syntax}:`

    .. code-block:: fortran

      RESULT = REAL(A [, KIND])
      RESULT = REALPART(Z)

  :samp:`{Example}:`

    .. code-block:: fortran

      program test_real
        complex :: x = (1.0, 2.0)
        print *, real(x), real(x,8), realpart(x)
      end program test_real

  :samp:`{Specific names}:`
    =============  ==============  ===========  =====================
    Name           Argument        Return type  Standard
    =============  ==============  ===========  =====================
    ``FLOAT(A)``   ``INTEGER(4)``  ``REAL(4)``  Fortran 77 and later
    ``DFLOAT(A)``  ``INTEGER(4)``  ``REAL(8)``  GNU extension
    ``FLOATI(A)``  ``INTEGER(2)``  ``REAL(4)``  GNU extension (-fdec)
    ``FLOATJ(A)``  ``INTEGER(4)``  ``REAL(4)``  GNU extension (-fdec)
    ``FLOATK(A)``  ``INTEGER(8)``  ``REAL(4)``  GNU extension (-fdec)
    ``SNGL(A)``    ``REAL(8)``     ``REAL(4)``  Fortran 77 and later
    =============  ==============  ===========  =====================

  :samp:`{See also}:`
    :ref:`DBLE`