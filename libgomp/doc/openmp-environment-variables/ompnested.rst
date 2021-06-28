..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

  .. _omp_nested:

OMP_NESTED -- Nested parallel regions
*************************************

.. index:: Environment Variable

.. index:: Implementation specific setting

:samp:`{Description}:`
  Enable or disable nested parallel regions, i.e., whether team members
  are allowed to create new teams.  The value of this environment variable
  shall be ``TRUE`` or ``FALSE``.  If set to ``TRUE``, the number
  of maximum active nested regions supported will by default be set to the
  maximum supported, otherwise it will be set to one.  If
  :envvar:`OMP_MAX_ACTIVE_LEVELS` is defined, its setting will override this
  setting.  If both are undefined, nested parallel regions are enabled if
  :envvar:`OMP_NUM_THREADS` or :envvar:`OMP_PROC_BINDS` are defined to a list with
  more than one item, otherwise they are disabled by default.

:samp:`{See also}:`
  omp_set_max_active_levels, omp_set_nested

:samp:`{Reference}:`
  `OpenMP specification v4.5 <https://www.openmp.org>`_, Section 4.6