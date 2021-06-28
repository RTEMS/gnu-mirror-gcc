..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

  .. _omp_get_ancestor_thread_num:

omp_get_ancestor_thread_num -- Ancestor thread ID
*************************************************

:samp:`{Description}:`
  This function returns the thread identification number for the given
  nesting level of the current thread.  For values of :samp:`{level}` outside
  zero to ``omp_get_level`` -1 is returned; if :samp:`{level}` is
  ``omp_get_level`` the result is identical to ``omp_get_thread_num``.

C/C++

  ============  ===============================================
  *Prototype*:  ``int omp_get_ancestor_thread_num(int level);``
  ============  ===============================================

:samp:`{Fortran}:`

  ============  =======================================================
  *Interface*:  ``integer function omp_get_ancestor_thread_num(level)``
                ``integer level``
  ============  =======================================================

:samp:`{See also}:`
  omp_get_level, omp_get_thread_num, omp_get_team_size

:samp:`{Reference}:`
  `OpenMP specification v4.5 <https://www.openmp.org>`_, Section 3.2.18.