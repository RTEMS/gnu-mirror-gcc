..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

  .. _omp_wait_policy:

OMP_WAIT_POLICY -- How waiting threads are handled
**************************************************

.. index:: Environment Variable

:samp:`{Description}:`
  Specifies whether waiting threads should be active or passive.  If
  the value is ``PASSIVE``, waiting threads should not consume CPU
  power while waiting; while the value is ``ACTIVE`` specifies that
  they should.  If undefined, threads wait actively for a short time
  before waiting passively.

:samp:`{See also}:`
  GOMP_SPINCOUNT

:samp:`{Reference}:`
  `OpenMP specification v4.5 <https://www.openmp.org>`_, Section 4.8