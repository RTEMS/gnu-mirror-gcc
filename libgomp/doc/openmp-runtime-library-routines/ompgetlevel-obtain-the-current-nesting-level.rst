..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

  .. _omp_get_level:

omp_get_level -- Obtain the current nesting level
*************************************************

:samp:`{Description}:`
  This function returns the nesting level for the parallel blocks,
  which enclose the calling call.

C/C++

  ============  ============================
  *Prototype*:  ``int omp_get_level(void);``
  ============  ============================

:samp:`{Fortran}:`

  ============  ================================
  *Interface*:  ``integer function omp_level()``
  ============  ================================

:samp:`{See also}:`
  omp_get_active_level

:samp:`{Reference}:`
  `OpenMP specification v4.5 <https://www.openmp.org>`_, Section 3.2.17.