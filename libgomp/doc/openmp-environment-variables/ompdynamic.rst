..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

  .. _omp_dynamic:

OMP_DYNAMIC -- Dynamic adjustment of threads
********************************************

.. index:: Environment Variable

:samp:`{Description}:`
  Enable or disable the dynamic adjustment of the number of threads
  within a team.  The value of this environment variable shall be
  ``TRUE`` or ``FALSE``.  If undefined, dynamic adjustment is
  disabled by default.

:samp:`{See also}:`
  omp_set_dynamic

:samp:`{Reference}:`
  `OpenMP specification v4.5 <https://www.openmp.org>`_, Section 4.3