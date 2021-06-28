..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

  .. _omp_thread_limit:

OMP_THREAD_LIMIT -- Set the maximum number of threads
*****************************************************

.. index:: Environment Variable

:samp:`{Description}:`
  Specifies the number of threads to use for the whole program.  The
  value of this variable shall be a positive integer.  If undefined,
  the number of threads is not limited.

:samp:`{See also}:`
  OMP_NUM_THREADS, omp_get_thread_limit

:samp:`{Reference}:`
  `OpenMP specification v4.5 <https://www.openmp.org>`_, Section 4.10