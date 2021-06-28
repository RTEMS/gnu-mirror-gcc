..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _irand:

IRAND --- Integer pseudo-random number
**************************************

.. index:: IRAND

.. index:: random number generation

.. function:: IRAND(FLAG)

  ``IRAND(FLAG)`` returns a pseudo-random number from a uniform
  distribution between 0 and a system-dependent limit (which is in most
  cases 2147483647). If :samp:`{FLAG}` is 0, the next number
  in the current sequence is returned; if :samp:`{FLAG}` is 1, the generator
  is restarted by ``CALL SRAND(0)`` ; if :samp:`{FLAG}` has any other value,
  it is used as a new seed with ``SRAND``.

  :param I:
    Shall be a scalar ``INTEGER`` of kind 4.

  :return:
    The return value is of ``INTEGER(kind=4)`` type.

  :samp:`{Standard}:`
    GNU extension

  :samp:`{Class}:`
    Function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    RESULT = IRAND(I)

  :samp:`{Example}:`

    .. code-block:: fortran

      program test_irand
        integer,parameter :: seed = 86456

        call srand(seed)
        print *, irand(), irand(), irand(), irand()
        print *, irand(seed), irand(), irand(), irand()
      end program test_irand