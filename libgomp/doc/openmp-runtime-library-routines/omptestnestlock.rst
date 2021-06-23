..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

  .. _omp_test_nest_lock:

omp_test_nest_lock -- Test and set nested lock if available
***********************************************************

:samp:`{Description}:`
  Before setting a nested lock, the lock variable must be initialized by
  ``omp_init_nest_lock``.  Contrary to ``omp_set_nest_lock``,
  ``omp_test_nest_lock`` does not block if the lock is not available.
  If the lock is already held by the current thread, the new nesting count
  is returned.  Otherwise, the return value equals zero.

:samp:`{C/C++}:`

  ============  ==================================================
  *Prototype*:  ``int omp_test_nest_lock(omp_nest_lock_t *lock);``
  ============  ==================================================

:samp:`{Fortran}:`

  ============  ======================================================
  *Interface*:  ``logical function omp_test_nest_lock(nvar)``
                ``integer(omp_nest_lock_kind), intent(inout) :: nvar``
  ============  ======================================================

:samp:`{See also}:`
  omp_init_lock, omp_set_lock, omp_set_lock

:samp:`{Reference}:`
  `OpenMP specification v4.5 <https://www.openmp.org>`_, Section 3.3.6.