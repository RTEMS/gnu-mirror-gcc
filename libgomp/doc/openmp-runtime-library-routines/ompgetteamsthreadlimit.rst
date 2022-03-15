..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _omp_get_teams_thread_limit:

omp_get_teams_thread_limit -- Maximum number of threads imposed by teams
************************************************************************

:samp:`{Description}:`
  Return the maximum number of threads that will be able to participate in
  each team created by a teams construct.

:samp:`{C/C++}:`

  ============  =========================================
  *Prototype*:  ``int omp_get_teams_thread_limit(void);``
  ============  =========================================

:samp:`{Fortran}:`

  ============  =================================================
  *Interface*:  ``integer function omp_get_teams_thread_limit()``
  ============  =================================================

:samp:`{See also}:`
  :ref:`omp_set_teams_thread_limit`, :ref:`OMP_TEAMS_THREAD_LIMIT`

:samp:`{Reference}:`
  `OpenMP specification v5.1 <https://www.openmp.org>`_, Section 3.4.6.