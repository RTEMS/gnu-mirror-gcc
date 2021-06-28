..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

  .. _omp_test_lock:

omp_test_lock -- Test and set simple lock if available
******************************************************

:samp:`{Description}:`
  Before setting a simple lock, the lock variable must be initialized by
  ``omp_init_lock``.  Contrary to ``omp_set_lock``, ``omp_test_lock``
  does not block if the lock is not available.  This function returns
  ``true`` upon success, ``false`` otherwise.  Here, ``true`` and
  ``false`` represent their language-specific counterparts.

:samp:`{C/C++}:`

  ============  ========================================
  *Prototype*:  ``int omp_test_lock(omp_lock_t *lock);``
  ============  ========================================

:samp:`{Fortran}:`

  ============  =================================================
  *Interface*:  ``logical function omp_test_lock(svar)``
                ``integer(omp_lock_kind), intent(inout) :: svar``
  ============  =================================================

:samp:`{See also}:`
  omp_init_lock, omp_set_lock, omp_set_lock

:samp:`{Reference}:`
  `OpenMP specification v4.5 <https://www.openmp.org>`_, Section 3.3.6.