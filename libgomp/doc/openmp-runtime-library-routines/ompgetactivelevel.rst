..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _omp_get_active_level:

omp_get_active_level -- Number of parallel regions
**************************************************

:samp:`{Description}:`
  This function returns the nesting level for the active parallel blocks,
  which enclose the calling call.

C/C++

  ============  ===================================
  *Prototype*:  ``int omp_get_active_level(void);``
  ============  ===================================

:samp:`{Fortran}:`

  ============  ===========================================
  *Interface*:  ``integer function omp_get_active_level()``
  ============  ===========================================

:samp:`{See also}:`
  omp_get_level, omp_get_max_active_levels, omp_set_max_active_levels

:samp:`{Reference}:`
  `OpenMP specification v4.5 <https://www.openmp.org>`_, Section 3.2.20.