..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

  .. _omp_is_initial_device:

omp_is_initial_device -- Whether executing on the host device
*************************************************************

:samp:`{Description}:`
  This function returns ``true`` if currently running on the host device,
  ``false`` otherwise.  Here, ``true`` and ``false`` represent
  their language-specific counterparts.

:samp:`{C/C++}:`

  ============  ====================================
  *Prototype*:  ``int omp_is_initial_device(void);``
  ============  ====================================

:samp:`{Fortran}:`

  ============  ============================================
  *Interface*:  ``logical function omp_is_initial_device()``
  ============  ============================================

:samp:`{Reference}:`
  `OpenMP specification v4.5 <https://www.openmp.org>`_, Section 3.2.34.