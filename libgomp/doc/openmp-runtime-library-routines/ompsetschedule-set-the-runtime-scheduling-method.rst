..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

  .. _omp_set_schedule:

omp_set_schedule -- Set the runtime scheduling method
*****************************************************

:samp:`{Description}:`
  Sets the runtime scheduling method.  The :samp:`{kind}` argument can have the
  value ``omp_sched_static``, ``omp_sched_dynamic``,
  ``omp_sched_guided`` or ``omp_sched_auto``.  Except for
  ``omp_sched_auto``, the chunk size is set to the value of
  :samp:`{chunk_size}` if positive, or to the default value if zero or negative.
  For ``omp_sched_auto`` the :samp:`{chunk_size}` argument is ignored.

C/C++

  ============  ============================================================
  *Prototype*:  ``void omp_set_schedule(omp_sched_t kind, int chunk_size);``
  ============  ============================================================

:samp:`{Fortran}:`

  ============  =================================================
  *Interface*:  ``subroutine omp_set_schedule(kind, chunk_size)``
                ``integer(kind=omp_sched_kind) kind``
                ``integer chunk_size``
  ============  =================================================

:samp:`{See also}:`
  omp_get_schedule
  OMP_SCHEDULE

:samp:`{Reference}:`
  `OpenMP specification v4.5 <https://www.openmp.org>`_, Section 3.2.12.