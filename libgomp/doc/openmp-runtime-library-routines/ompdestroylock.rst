..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

  .. _omp_destroy_lock:

omp_destroy_lock -- Destroy simple lock
***************************************

:samp:`{Description}:`
  Destroy a simple lock.  In order to be destroyed, a simple lock must be
  in the unlocked state.

:samp:`{C/C++}:`

  ============  ============================================
  *Prototype*:  ``void omp_destroy_lock(omp_lock_t *lock);``
  ============  ============================================

:samp:`{Fortran}:`

  ============  =================================================
  *Interface*:  ``subroutine omp_destroy_lock(svar)``
                ``integer(omp_lock_kind), intent(inout) :: svar``
  ============  =================================================

:samp:`{See also}:`
  omp_init_lock

:samp:`{Reference}:`
  `OpenMP specification v4.5 <https://www.openmp.org>`_, Section 3.3.3.