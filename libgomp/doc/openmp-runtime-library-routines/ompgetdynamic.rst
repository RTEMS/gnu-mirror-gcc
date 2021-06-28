..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

  .. _omp_get_dynamic:

omp_get_dynamic -- Dynamic teams setting
****************************************

:samp:`{Description}:`
  This function returns ``true`` if enabled, ``false`` otherwise.
  Here, ``true`` and ``false`` represent their language-specific
  counterparts.

  The dynamic team setting may be initialized at startup by the
  :envvar:`OMP_DYNAMIC` environment variable or at runtime using
  ``omp_set_dynamic``.  If undefined, dynamic adjustment is
  disabled by default.

:samp:`{C/C++}:`

  ============  ==============================
  *Prototype*:  ``int omp_get_dynamic(void);``
  ============  ==============================

:samp:`{Fortran}:`

  ============  ======================================
  *Interface*:  ``logical function omp_get_dynamic()``
  ============  ======================================

:samp:`{See also}:`
  omp_set_dynamic, OMP_DYNAMIC

:samp:`{Reference}:`
  `OpenMP specification v4.5 <https://www.openmp.org>`_, Section 3.2.8.