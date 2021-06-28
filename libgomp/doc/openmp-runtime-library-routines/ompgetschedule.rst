..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

  .. _omp_get_schedule:

omp_get_schedule -- Obtain the runtime scheduling method
********************************************************

:samp:`{Description}:`
  Obtain the runtime scheduling method.  The :samp:`{kind}` argument will be
  set to the value ``omp_sched_static``, ``omp_sched_dynamic``,
  ``omp_sched_guided`` or ``omp_sched_auto``.  The second argument,
  :samp:`{chunk_size}`, is set to the chunk size.

C/C++

  ============  ==============================================================
  *Prototype*:  ``void omp_get_schedule(omp_sched_t *kind, int *chunk_size);``
  ============  ==============================================================

:samp:`{Fortran}:`

  ============  =================================================
  *Interface*:  ``subroutine omp_get_schedule(kind, chunk_size)``
                ``integer(kind=omp_sched_kind) kind``
                ``integer chunk_size``
  ============  =================================================

:samp:`{See also}:`
  omp_set_schedule, OMP_SCHEDULE

:samp:`{Reference}:`
  `OpenMP specification v4.5 <https://www.openmp.org>`_, Section 3.2.13.