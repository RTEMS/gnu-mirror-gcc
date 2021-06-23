..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

  .. _omp_get_num_teams:

omp_get_num_teams -- Number of teams
************************************

:samp:`{Description}:`
  Returns the number of teams in the current team region.

:samp:`{C/C++}:`

  ============  ================================
  *Prototype*:  ``int omp_get_num_teams(void);``
  ============  ================================

:samp:`{Fortran}:`

  ============  ========================================
  *Interface*:  ``integer function omp_get_num_teams()``
  ============  ========================================

:samp:`{Reference}:`
  `OpenMP specification v4.5 <https://www.openmp.org>`_, Section 3.2.32.