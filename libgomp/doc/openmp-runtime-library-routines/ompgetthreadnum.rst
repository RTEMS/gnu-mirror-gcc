..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

  .. _omp_get_thread_num:

omp_get_thread_num -- Current thread ID
***************************************

:samp:`{Description}:`
  Returns a unique thread identification number within the current team.
  In a sequential parts of the program, ``omp_get_thread_num``
  always returns 0.  In parallel regions the return value varies
  from 0 to ``omp_get_num_threads`` -1 inclusive.  The return
  value of the master thread of a team is always 0.

:samp:`{C/C++}:`

  ============  =================================
  *Prototype*:  ``int omp_get_thread_num(void);``
  ============  =================================

:samp:`{Fortran}:`

  ============  =========================================
  *Interface*:  ``integer function omp_get_thread_num()``
  ============  =========================================

:samp:`{See also}:`
  omp_get_num_threads, omp_get_ancestor_thread_num

:samp:`{Reference}:`
  `OpenMP specification v4.5 <https://www.openmp.org>`_, Section 3.2.4.