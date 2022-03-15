..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _omp_get_max_teams:

omp_get_max_teams -- Maximum number of teams of teams region
************************************************************

:samp:`{Description}:`
  Return the maximum number of teams used for the teams region
  that does not use the clause ``num_teams``.

:samp:`{C/C++}:`

  ============  ================================
  *Prototype*:  ``int omp_get_max_teams(void);``
  ============  ================================

:samp:`{Fortran}:`

  ============  ========================================
  *Interface*:  ``integer function omp_get_max_teams()``
  ============  ========================================

:samp:`{See also}:`
  :ref:`omp_set_num_teams`, :ref:`omp_get_num_teams`

:samp:`{Reference}:`
  `OpenMP specification v5.1 <https://www.openmp.org>`_, Section 3.4.4.