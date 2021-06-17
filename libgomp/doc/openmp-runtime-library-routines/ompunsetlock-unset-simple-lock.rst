..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

  .. _omp_unset_lock:

omp_unset_lock -- Unset simple lock
***********************************

:samp:`{Description}:`
  A simple lock about to be unset must have been locked by ``omp_set_lock``
  or ``omp_test_lock`` before.  In addition, the lock must be held by the
  thread calling ``omp_unset_lock``.  Then, the lock becomes unlocked.  If one
  or more threads attempted to set the lock before, one of them is chosen to,
  again, set the lock to itself.

:samp:`{C/C++}:`

  ============  ==========================================
  *Prototype*:  ``void omp_unset_lock(omp_lock_t *lock);``
  ============  ==========================================

:samp:`{Fortran}:`

  ============  =================================================
  *Interface*:  ``subroutine omp_unset_lock(svar)``
                ``integer(omp_lock_kind), intent(inout) :: svar``
  ============  =================================================

:samp:`{See also}:`
  omp_set_lock, omp_test_lock

:samp:`{Reference}:`
  `OpenMP specification v4.5 <https://www.openmp.org>`_, Section 3.3.5.