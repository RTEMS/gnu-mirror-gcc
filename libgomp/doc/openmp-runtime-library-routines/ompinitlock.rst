..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

  .. _omp_init_lock:

omp_init_lock -- Initialize simple lock
***************************************

:samp:`{Description}:`
  Initialize a simple lock.  After initialization, the lock is in
  an unlocked state.

:samp:`{C/C++}:`

  ============  =========================================
  *Prototype*:  ``void omp_init_lock(omp_lock_t *lock);``
  ============  =========================================

:samp:`{Fortran}:`

  ============  ===============================================
  *Interface*:  ``subroutine omp_init_lock(svar)``
                ``integer(omp_lock_kind), intent(out) :: svar``
  ============  ===============================================

:samp:`{See also}:`
  omp_destroy_lock

:samp:`{Reference}:`
  `OpenMP specification v4.5 <https://www.openmp.org>`_, Section 3.3.1.