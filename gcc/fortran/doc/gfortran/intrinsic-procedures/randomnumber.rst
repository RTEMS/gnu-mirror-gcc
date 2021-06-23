..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _random_number:

RANDOM_NUMBER --- Pseudo-random number
**************************************

.. index:: RANDOM_NUMBER

.. index:: random number generation

.. function:: RANDOM_NUMBER

  Returns a single pseudorandom number or an array of pseudorandom numbers
  from the uniform distribution over the range 0 \leq x < 1.

  :param HARVEST:
    Shall be a scalar or an array of type ``REAL``.

  :samp:`{Standard}:`
    Fortran 90 and later

  :samp:`{Class}:`
    Subroutine

  :samp:`{Syntax}:`

  .. code-block:: fortran

    CALL RANDOM_NUMBER(HARVEST)

  :samp:`{Example}:`

    .. code-block:: fortran

      program test_random_number
        REAL :: r(5,5)
        CALL RANDOM_NUMBER(r)
      end program

  :samp:`{See also}:`
    RANDOM_SEED,
    RANDOM_INIT