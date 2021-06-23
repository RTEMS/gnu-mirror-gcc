..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

  .. _omp_get_nested:

omp_get_nested -- Nested parallel regions
*****************************************

:samp:`{Description}:`
  This function returns ``true`` if nested parallel regions are
  enabled, ``false`` otherwise.  Here, ``true`` and ``false``
  represent their language-specific counterparts.

  The state of nested parallel regions at startup depends on several
  environment variables.  If :envvar:`OMP_MAX_ACTIVE_LEVELS` is defined
  and is set to greater than one, then nested parallel regions will be
  enabled.  If not defined, then the value of the :envvar:`OMP_NESTED`
  environment variable will be followed if defined.  If neither are
  defined, then if either :envvar:`OMP_NUM_THREADS` or :envvar:`OMP_PROC_BIND`
  are defined with a list of more than one value, then nested parallel
  regions are enabled.  If none of these are defined, then nested parallel
  regions are disabled by default.

  Nested parallel regions can be enabled or disabled at runtime using
  ``omp_set_nested``, or by setting the maximum number of nested
  regions with ``omp_set_max_active_levels`` to one to disable, or
  above one to enable.

:samp:`{C/C++}:`

  ============  =============================
  *Prototype*:  ``int omp_get_nested(void);``
  ============  =============================

:samp:`{Fortran}:`

  ============  =====================================
  *Interface*:  ``logical function omp_get_nested()``
  ============  =====================================

:samp:`{See also}:`
  omp_set_max_active_levels, omp_set_nested,
  OMP_MAX_ACTIVE_LEVELS, OMP_NESTED

:samp:`{Reference}:`
  `OpenMP specification v4.5 <https://www.openmp.org>`_, Section 3.2.11.