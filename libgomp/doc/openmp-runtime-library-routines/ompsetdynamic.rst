..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

  .. _omp_set_dynamic:

omp_set_dynamic -- Enable/disable dynamic teams
***********************************************

:samp:`{Description}:`
  Enable or disable the dynamic adjustment of the number of threads
  within a team.  The function takes the language-specific equivalent
  of ``true`` and ``false``, where ``true`` enables dynamic
  adjustment of team sizes and ``false`` disables it.

:samp:`{C/C++}:`

  ============  ==============================================
  *Prototype*:  ``void omp_set_dynamic(int dynamic_threads);``
  ============  ==============================================

:samp:`{Fortran}:`

  ============  ===============================================
  *Interface*:  ``subroutine omp_set_dynamic(dynamic_threads)``
                ``logical, intent(in) :: dynamic_threads``
  ============  ===============================================

:samp:`{See also}:`
  OMP_DYNAMIC, omp_get_dynamic

:samp:`{Reference}:`
  `OpenMP specification v4.5 <https://www.openmp.org>`_, Section 3.2.7.