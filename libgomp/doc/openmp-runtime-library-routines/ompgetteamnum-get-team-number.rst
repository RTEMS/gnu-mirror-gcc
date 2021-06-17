..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

  .. _omp_get_team_num:

omp_get_team_num -- Get team number
***********************************

:samp:`{Description}:`
  Returns the team number of the calling thread.

:samp:`{C/C++}:`

  ============  ===============================
  *Prototype*:  ``int omp_get_team_num(void);``
  ============  ===============================

:samp:`{Fortran}:`

  ============  =======================================
  *Interface*:  ``integer function omp_get_team_num()``
  ============  =======================================

:samp:`{Reference}:`
  `OpenMP specification v4.5 <https://www.openmp.org>`_, Section 3.2.33.