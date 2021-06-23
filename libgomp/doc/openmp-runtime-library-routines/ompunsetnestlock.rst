..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

  .. _omp_unset_nest_lock:

omp_unset_nest_lock -- Unset nested lock
****************************************

:samp:`{Description}:`
  A nested lock about to be unset must have been locked by ``omp_set_nested_lock``
  or ``omp_test_nested_lock`` before.  In addition, the lock must be held by the
  thread calling ``omp_unset_nested_lock``.  If the nesting count drops to zero, the
  lock becomes unlocked.  If one ore more threads attempted to set the lock before,
  one of them is chosen to, again, set the lock to itself.

:samp:`{C/C++}:`

  ============  ====================================================
  *Prototype*:  ``void omp_unset_nest_lock(omp_nest_lock_t *lock);``
  ============  ====================================================

:samp:`{Fortran}:`

  ============  ======================================================
  *Interface*:  ``subroutine omp_unset_nest_lock(nvar)``
                ``integer(omp_nest_lock_kind), intent(inout) :: nvar``
  ============  ======================================================

:samp:`{See also}:`
  omp_set_nest_lock

:samp:`{Reference}:`
  `OpenMP specification v4.5 <https://www.openmp.org>`_, Section 3.3.5.