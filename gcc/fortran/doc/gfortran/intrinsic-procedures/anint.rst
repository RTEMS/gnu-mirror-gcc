..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _anint:

ANINT --- Nearest whole number
******************************

.. index:: ANINT

.. index:: DNINT

.. index:: ceiling

.. index:: rounding, ceiling

.. function:: ANINT(A [, KIND])

  ``ANINT(A [, KIND])`` rounds its argument to the nearest whole number.

  :param A:
    The type of the argument shall be ``REAL``.

  :param KIND:
    (Optional) An ``INTEGER`` initialization
    expression indicating the kind parameter of the result.

  :return:
    The return value is of type real with the kind type parameter of the
    argument if the optional :samp:`{KIND}` is absent; otherwise, the kind
    type parameter will be given by :samp:`{KIND}`.  If :samp:`{A}` is greater than
    zero, ``ANINT(A)`` returns ``AINT(X+0.5)``.  If :samp:`{A}` is
    less than or equal to zero then it returns ``AINT(X-0.5)``.

  :samp:`{Standard}:`
    Fortran 77 and later

  :samp:`{Class}:`
    Elemental function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    RESULT = ANINT(A [, KIND])

  :samp:`{Example}:`

    .. code-block:: fortran

      program test_anint
        real(4) x4
        real(8) x8
        x4 = 1.234E0_4
        x8 = 4.321_8
        print *, anint(x4), dnint(x8)
        x8 = anint(x4,8)
      end program test_anint

  :samp:`{Specific names}:`
    ============  =============  ===========  ====================
    Name          Argument       Return type  Standard
    ============  =============  ===========  ====================
    ``ANINT(A)``  ``REAL(4) A``  ``REAL(4)``  Fortran 77 and later
    ``DNINT(A)``  ``REAL(8) A``  ``REAL(8)``  Fortran 77 and later
    ============  =============  ===========  ====================