..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

  .. _omp_get_num_procs:

omp_get_num_procs -- Number of processors online
************************************************

:samp:`{Description}:`
  Returns the number of processors online on that device.

:samp:`{C/C++}:`

  ============  ================================
  *Prototype*:  ``int omp_get_num_procs(void);``
  ============  ================================

:samp:`{Fortran}:`

  ============  ========================================
  *Interface*:  ``integer function omp_get_num_procs()``
  ============  ========================================

:samp:`{Reference}:`
  `OpenMP specification v4.5 <https://www.openmp.org>`_, Section 3.2.5.