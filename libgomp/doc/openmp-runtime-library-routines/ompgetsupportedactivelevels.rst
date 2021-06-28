..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

  .. _omp_get_supported_active_levels:

omp_get_supported_active_levels -- Maximum number of active regions supported
*****************************************************************************

:samp:`{Description}:`
  This function returns the maximum number of nested, active parallel regions
  supported by this implementation.

C/C++

  ============  ==============================================
  *Prototype*:  ``int omp_get_supported_active_levels(void);``
  ============  ==============================================

:samp:`{Fortran}:`

  ============  ======================================================
  *Interface*:  ``integer function omp_get_supported_active_levels()``
  ============  ======================================================

:samp:`{See also}:`
  omp_get_max_active_levels, omp_set_max_active_levels

:samp:`{Reference}:`
  `OpenMP specification v5.0 <https://www.openmp.org>`_, Section 3.2.15.