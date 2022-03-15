..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. index:: Environment Variable

.. _omp_teams_thread_limit:

OMP_TEAMS_THREAD_LIMIT -- Set the maximum number of threads imposed by teams
****************************************************************************

:samp:`{Description}:`
  Specifies an upper bound for the number of threads to use by each contention
  group created by a teams construct without explicit ``thread_limit``
  clause.  The value of this variable shall be a positive integer.  If undefined,
  the value of 0 is used which stands for an implementation defined upper
  limit.

:samp:`{See also}:`
  :ref:`OMP_THREAD_LIMIT`, :ref:`omp_set_teams_thread_limit`

:samp:`{Reference}`:
  `OpenMP specification v5.1 <https://www.openmp.org>`_, Section 6.24