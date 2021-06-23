..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

  .. _omp_set_num_threads:

omp_set_num_threads -- Set upper team size limit
************************************************

:samp:`{Description}:`
  Specifies the number of threads used by default in subsequent parallel
  sections, if those do not specify a ``num_threads`` clause.  The
  argument of ``omp_set_num_threads`` shall be a positive integer.

:samp:`{C/C++}:`

  ============  ==============================================
  *Prototype*:  ``void omp_set_num_threads(int num_threads);``
  ============  ==============================================

:samp:`{Fortran}:`

  ============  ===============================================
  *Interface*:  ``subroutine omp_set_num_threads(num_threads)``
                ``integer, intent(in) :: num_threads``
  ============  ===============================================

:samp:`{See also}:`
  OMP_NUM_THREADS, omp_get_num_threads, omp_get_max_threads

:samp:`{Reference}:`
  `OpenMP specification v4.5 <https://www.openmp.org>`_, Section 3.2.1.