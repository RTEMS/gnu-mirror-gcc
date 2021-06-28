..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

GIMPLE_OMP_ORDERED
^^^^^^^^^^^^^^^^^^

.. index:: GIMPLE_OMP_ORDERED

.. function:: gimple gimple_build_omp_ordered (gimple_seq body)

  Build a ``GIMPLE_OMP_ORDERED`` statement.

``BODY`` is the sequence of statements inside a loop that will
executed in sequence.