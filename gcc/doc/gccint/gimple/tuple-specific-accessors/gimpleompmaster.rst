..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

GIMPLE_OMP_MASTER
^^^^^^^^^^^^^^^^^

.. index:: GIMPLE_OMP_MASTER

.. function:: gimple gimple_build_omp_master (gimple_seq body)

  Build a ``GIMPLE_OMP_MASTER`` statement. ``BODY`` is the sequence of
  statements to be executed by just the master.