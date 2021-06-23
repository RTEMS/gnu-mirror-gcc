..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _random_seed:

RANDOM_SEED --- Initialize a pseudo-random number sequence
**********************************************************

.. index:: RANDOM_SEED

.. index:: random number generation, seeding

.. index:: seeding a random number generator

.. function:: RANDOM_SEED

  Restarts or queries the state of the pseudorandom number generator used by
  ``RANDOM_NUMBER``.

  :param SIZE:
    (Optional) Shall be a scalar and of type default
    ``INTEGER``, with ``INTENT(OUT)``. It specifies the minimum size
    of the arrays used with the :samp:`{PUT}` and :samp:`{GET}` arguments.

  :param PUT:
    (Optional) Shall be an array of type default
    ``INTEGER`` and rank one. It is ``INTENT(IN)`` and the size of
    the array must be larger than or equal to the number returned by the
    :samp:`{SIZE}` argument.

  :param GET:
    (Optional) Shall be an array of type default
    ``INTEGER`` and rank one. It is ``INTENT(OUT)`` and the size
    of the array must be larger than or equal to the number returned by
    the :samp:`{SIZE}` argument.

  :samp:`{Standard}:`
    Fortran 90 and later

  :samp:`{Class}:`
    Subroutine

  :samp:`{Syntax}:`

  .. code-block:: fortran

    CALL RANDOM_SEED([SIZE, PUT, GET])

  :samp:`{Example}:`

    .. code-block:: fortran

      program test_random_seed
        implicit none
        integer, allocatable :: seed(:)
        integer :: n

        call random_seed(size = n)
        allocate(seed(n))
        call random_seed(get=seed)
        write (*, *) seed
      end program test_random_seed

  :samp:`{See also}:`
    RANDOM_NUMBER,
    RANDOM_INIT