..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _srand:

SRAND --- Reinitialize the random number generator
**************************************************

.. index:: SRAND

.. index:: random number generation, seeding

.. index:: seeding a random number generator

.. function:: SRAND

  ``SRAND`` reinitializes the pseudo-random number generator
  called by ``RAND`` and ``IRAND``. The new seed used by the
  generator is specified by the required argument :samp:`{SEED}`.

  :param SEED:
    Shall be a scalar ``INTEGER(kind=4)``.

  :return:
    Does not return anything.

  :samp:`{Standard}:`
    GNU extension

  :samp:`{Class}:`
    Subroutine

  :samp:`{Syntax}:`

  .. code-block:: fortran

    CALL SRAND(SEED)

  :samp:`{Example}:`
    See ``RAND`` and ``IRAND`` for examples.

  :samp:`{Notes}:`
    The Fortran standard specifies the intrinsic subroutines
    ``RANDOM_SEED`` to initialize the pseudo-random number
    generator and ``RANDOM_NUMBER`` to generate pseudo-random numbers.
    These subroutines should be used in new codes.

    Please note that in GNU Fortran, these two sets of intrinsics ( ``RAND``,
    ``IRAND`` and ``SRAND`` on the one hand, ``RANDOM_NUMBER`` and
    ``RANDOM_SEED`` on the other hand) access two independent
    pseudo-random number generators.

  :samp:`{See also}:`
    RAND,
    RANDOM_SEED,
    RANDOM_NUMBER