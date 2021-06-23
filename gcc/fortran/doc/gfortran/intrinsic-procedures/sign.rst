..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _sign:

SIGN --- Sign copying function
******************************

.. index:: SIGN

.. index:: ISIGN

.. index:: DSIGN

.. index:: sign copying

.. function:: SIGN(A,B)

  ``SIGN(A,B)`` returns the value of :samp:`{A}` with the sign of :samp:`{B}`.

  :param A:
    Shall be of type ``INTEGER`` or ``REAL``

  :param B:
    Shall be of the same type and kind as :samp:`{A}`.

  :return:
    The kind of the return value is that of :samp:`{A}` and :samp:`{B}`.
    If B\ge 0 then the result is ``ABS(A)``, else
    it is ``-ABS(A)``.

  :samp:`{Standard}:`
    Fortran 77 and later

  :samp:`{Class}:`
    Elemental function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    RESULT = SIGN(A, B)

  :samp:`{Example}:`

    .. code-block:: fortran

      program test_sign
        print *, sign(-12,1)
        print *, sign(-12,0)
        print *, sign(-12,-1)

        print *, sign(-12.,1.)
        print *, sign(-12.,0.)
        print *, sign(-12.,-1.)
      end program test_sign

  :samp:`{Specific names}:`
    ==============  ===================  ==============  ====================
    Name            Arguments            Return type     Standard
    ==============  ===================  ==============  ====================
    ``SIGN(A,B)``   ``REAL(4) A, B``     ``REAL(4)``     Fortran 77 and later
    ``ISIGN(A,B)``  ``INTEGER(4) A, B``  ``INTEGER(4)``  Fortran 77 and later
    ``DSIGN(A,B)``  ``REAL(8) A, B``     ``REAL(8)``     Fortran 77 and later
    ==============  ===================  ==============  ====================