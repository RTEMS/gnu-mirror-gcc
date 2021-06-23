..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

  .. _omp_get_num_threads:

omp_get_num_threads -- Size of the active team
**********************************************

:samp:`{Description}:`
  Returns the number of threads in the current team.  In a sequential section of
  the program ``omp_get_num_threads`` returns 1.

  The default team size may be initialized at startup by the
  :envvar:`OMP_NUM_THREADS` environment variable.  At runtime, the size
  of the current team may be set either by the ``NUM_THREADS``
  clause or by ``omp_set_num_threads``.  If none of the above were
  used to define a specific value and :envvar:`OMP_DYNAMIC` is disabled,
  one thread per CPU online is used.

:samp:`{C/C++}:`

  ============  ==================================
  *Prototype*:  ``int omp_get_num_threads(void);``
  ============  ==================================

:samp:`{Fortran}:`

  ============  ==========================================
  *Interface*:  ``integer function omp_get_num_threads()``
  ============  ==========================================

:samp:`{See also}:`
  omp_get_max_threads, omp_set_num_threads, OMP_NUM_THREADS

:samp:`{Reference}:`
  `OpenMP specification v4.5 <https://www.openmp.org>`_, Section 3.2.2.