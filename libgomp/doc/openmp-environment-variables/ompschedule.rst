..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

  .. _omp_schedule:

OMP_SCHEDULE -- How threads are scheduled
*****************************************

.. index:: Environment Variable

.. index:: Implementation specific setting

:samp:`{Description}:`
  Allows to specify ``schedule type`` and ``chunk size``.
  The value of the variable shall have the form: ``type[,chunk]`` where
  ``type`` is one of ``static``, ``dynamic``, ``guided`` or ``auto``
  The optional ``chunk`` size shall be a positive integer.  If undefined,
  dynamic scheduling and a chunk size of 1 is used.

:samp:`{See also}:`
  omp_set_schedule

:samp:`{Reference}:`
  `OpenMP specification v4.5 <https://www.openmp.org>`_, Sections 2.7.1.1 and 4.1