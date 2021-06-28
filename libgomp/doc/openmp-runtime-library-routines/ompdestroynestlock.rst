..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

  .. _omp_destroy_nest_lock:

omp_destroy_nest_lock -- Destroy nested lock
********************************************

:samp:`{Description}:`
  Destroy a nested lock.  In order to be destroyed, a nested lock must be
  in the unlocked state and its nesting count must equal zero.

:samp:`{C/C++}:`

  ============  ==================================================
  *Prototype*:  ``void omp_destroy_nest_lock(omp_nest_lock_t *);``
  ============  ==================================================

:samp:`{Fortran}:`

  ============  ======================================================
  *Interface*:  ``subroutine omp_destroy_nest_lock(nvar)``
                ``integer(omp_nest_lock_kind), intent(inout) :: nvar``
  ============  ======================================================

:samp:`{See also}:`
  omp_init_lock

:samp:`{Reference}:`
  `OpenMP specification v4.5 <https://www.openmp.org>`_, Section 3.3.3.