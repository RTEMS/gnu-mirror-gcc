..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

  .. _omp_init_nest_lock:

omp_init_nest_lock -- Initialize nested lock
********************************************

:samp:`{Description}:`
  Initialize a nested lock.  After initialization, the lock is in
  an unlocked state and the nesting count is set to zero.

:samp:`{C/C++}:`

  ============  ===================================================
  *Prototype*:  ``void omp_init_nest_lock(omp_nest_lock_t *lock);``
  ============  ===================================================

:samp:`{Fortran}:`

  ============  ====================================================
  *Interface*:  ``subroutine omp_init_nest_lock(nvar)``
                ``integer(omp_nest_lock_kind), intent(out) :: nvar``
  ============  ====================================================

:samp:`{See also}:`
  omp_destroy_nest_lock

:samp:`{Reference}:`
  `OpenMP specification v4.5 <https://www.openmp.org>`_, Section 3.3.1.