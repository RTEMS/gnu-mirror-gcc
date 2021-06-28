..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

  .. _omp_get_cancellation:

omp_get_cancellation -- Whether cancellation support is enabled
***************************************************************

:samp:`{Description}:`
  This function returns ``true`` if cancellation is activated, ``false``
  otherwise.  Here, ``true`` and ``false`` represent their language-specific
  counterparts.  Unless :envvar:`OMP_CANCELLATION` is set true, cancellations are
  deactivated.

:samp:`{C/C++}:`

  ============  ===================================
  *Prototype*:  ``int omp_get_cancellation(void);``
  ============  ===================================

:samp:`{Fortran}:`

  ============  ===========================================
  *Interface*:  ``logical function omp_get_cancellation()``
  ============  ===========================================

:samp:`{See also}:`
  OMP_CANCELLATION

:samp:`{Reference}:`
  `OpenMP specification v4.5 <https://www.openmp.org>`_, Section 3.2.9.