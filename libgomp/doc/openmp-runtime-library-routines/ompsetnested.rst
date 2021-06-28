..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

  .. _omp_set_nested:

omp_set_nested -- Enable/disable nested parallel regions
********************************************************

:samp:`{Description}:`
  Enable or disable nested parallel regions, i.e., whether team members
  are allowed to create new teams.  The function takes the language-specific
  equivalent of ``true`` and ``false``, where ``true`` enables
  dynamic adjustment of team sizes and ``false`` disables it.

  Enabling nested parallel regions will also set the maximum number of
  active nested regions to the maximum supported.  Disabling nested parallel
  regions will set the maximum number of active nested regions to one.

:samp:`{C/C++}:`

  ============  ====================================
  *Prototype*:  ``void omp_set_nested(int nested);``
  ============  ====================================

:samp:`{Fortran}:`

  ============  =====================================
  *Interface*:  ``subroutine omp_set_nested(nested)``
                ``logical, intent(in) :: nested``
  ============  =====================================

:samp:`{See also}:`
  omp_get_nested, omp_set_max_active_levels,
  OMP_MAX_ACTIVE_LEVELS, OMP_NESTED

:samp:`{Reference}:`
  `OpenMP specification v4.5 <https://www.openmp.org>`_, Section 3.2.10.