..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

  .. _omp_set_lock:

omp_set_lock -- Wait for and set simple lock
********************************************

:samp:`{Description}:`
  Before setting a simple lock, the lock variable must be initialized by
  ``omp_init_lock``.  The calling thread is blocked until the lock
  is available.  If the lock is already held by the current thread,
  a deadlock occurs.

:samp:`{C/C++}:`

  ============  ========================================
  *Prototype*:  ``void omp_set_lock(omp_lock_t *lock);``
  ============  ========================================

:samp:`{Fortran}:`

  ============  =================================================
  *Interface*:  ``subroutine omp_set_lock(svar)``
                ``integer(omp_lock_kind), intent(inout) :: svar``
  ============  =================================================

:samp:`{See also}:`
  omp_init_lock, omp_test_lock, omp_unset_lock

:samp:`{Reference}:`
  `OpenMP specification v4.5 <https://www.openmp.org>`_, Section 3.3.4.