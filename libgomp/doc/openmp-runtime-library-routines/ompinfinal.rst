..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

  .. _omp_in_final:

omp_in_final -- Whether in final or included task region
********************************************************

:samp:`{Description}:`
  This function returns ``true`` if currently running in a final
  or included task region, ``false`` otherwise.  Here, ``true``
  and ``false`` represent their language-specific counterparts.

:samp:`{C/C++}:`

  ============  ===========================
  *Prototype*:  ``int omp_in_final(void);``
  ============  ===========================

:samp:`{Fortran}:`

  ============  ===================================
  *Interface*:  ``logical function omp_in_final()``
  ============  ===================================

:samp:`{Reference}:`
  `OpenMP specification v4.5 <https://www.openmp.org>`_, Section 3.2.21.