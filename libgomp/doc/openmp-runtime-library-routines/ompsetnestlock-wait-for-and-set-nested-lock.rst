..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

  .. _omp_set_nest_lock:

omp_set_nest_lock -- Wait for and set nested lock
*************************************************

:samp:`{Description}:`
  Before setting a nested lock, the lock variable must be initialized by
  ``omp_init_nest_lock``.  The calling thread is blocked until the lock
  is available.  If the lock is already held by the current thread, the
  nesting count for the lock is incremented.

:samp:`{C/C++}:`

  ============  ==================================================
  *Prototype*:  ``void omp_set_nest_lock(omp_nest_lock_t *lock);``
  ============  ==================================================

:samp:`{Fortran}:`

  ============  ======================================================
  *Interface*:  ``subroutine omp_set_nest_lock(nvar)``
                ``integer(omp_nest_lock_kind), intent(inout) :: nvar``
  ============  ======================================================

:samp:`{See also}:`
  omp_init_nest_lock, omp_unset_nest_lock

:samp:`{Reference}:`
  `OpenMP specification v4.5 <https://www.openmp.org>`_, Section 3.3.4.