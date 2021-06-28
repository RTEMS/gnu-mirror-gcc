..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

  .. _omp_num_threads:

OMP_NUM_THREADS -- Specifies the number of threads to use
*********************************************************

.. index:: Environment Variable

.. index:: Implementation specific setting

:samp:`{Description}:`
  Specifies the default number of threads to use in parallel regions.  The
  value of this variable shall be a comma-separated list of positive integers;
  the value specifies the number of threads to use for the corresponding nested
  level.  Specifying more than one item in the list will automatically enable
  nesting by default.  If undefined one thread per CPU is used.

:samp:`{See also}:`
  omp_set_num_threads, OMP_NESTED

:samp:`{Reference}:`
  `OpenMP specification v4.5 <https://www.openmp.org>`_, Section 4.2