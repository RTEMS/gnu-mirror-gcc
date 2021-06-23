..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

  .. _omp_proc_bind:

OMP_PROC_BIND -- Whether theads may be moved between CPUs
*********************************************************

.. index:: Environment Variable

:samp:`{Description}:`
  Specifies whether threads may be moved between processors.  If set to
  ``TRUE``, OpenMP theads should not be moved; if set to ``FALSE``
  they may be moved.  Alternatively, a comma separated list with the
  values ``MASTER``, ``CLOSE`` and ``SPREAD`` can be used to specify
  the thread affinity policy for the corresponding nesting level.  With
  ``MASTER`` the worker threads are in the same place partition as the
  master thread.  With ``CLOSE`` those are kept close to the master thread
  in contiguous place partitions.  And with ``SPREAD`` a sparse distribution
  across the place partitions is used.  Specifying more than one item in the
  list will automatically enable nesting by default.

  When undefined, :envvar:`OMP_PROC_BIND` defaults to ``TRUE`` when
  :envvar:`OMP_PLACES` or :envvar:`GOMP_CPU_AFFINITY` is set and ``FALSE`` otherwise.

:samp:`{See also}:`
  omp_get_proc_bind, GOMP_CPU_AFFINITY,
  OMP_NESTED, OMP_PLACES

:samp:`{Reference}:`
  `OpenMP specification v4.5 <https://www.openmp.org>`_, Section 4.4